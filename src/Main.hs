{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lucid
    ( src_,
      script_,
      footer_,
      section_,
      h1_,
      article_,
      body_,
      href_,
      rel_,
      link_,
      name_,
      charset_,
      title_,
      content_,
      httpEquiv_,
      meta_,
      head_,
      html_,
      doctype_,
      renderText,
      ToHtml(toHtmlRaw, toHtml),
      Html )
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
-- Server stuff
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)
import Main.Utf8 (withUtf8)
import Data.Text
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Filter (applyFilters, Filter (CiteprocFilter))

main :: IO ()
main = withUtf8 $
  shakeArgs shakeOptions{shakeFiles=".shake"} $ do

  let inputDir = "content"
  let outputDir = "dist"

  -- To serve the generated files (useful for previewing),
  -- run `shake serve`.
  phony "serve" $
    liftIO $ serve 8080 outputDir

  -- Convert markdown to html
  action $ do
    srcs <- getDirectoryFiles inputDir ["//*.md"]
    let neededFiles = [outputDir </> src -<.> "html" | src <- srcs]
    -- liftIO $ print neededFiles
    need neededFiles

  -- Copy over images and assets as-is
  action $ do
    srcs <- getDirectoryFiles inputDir ["static//*"]
    need [outputDir </> src | src <- srcs]

  -- Copy over all static files
  "dist/static/**/*" %> \out -> do
    let source = inputDir </> dropDirectory1 out
    need [source]
    copyFileChanged source out

  -- Convert HTML files from markdown
  "dist//*.html" %> \out -> do
    let source = inputDir </> dropDirectory1 ( out -<.> "md" )
    let bib = inputDir </> "references.bib"
    need [source, bib]
    src <- liftIO $ TIO.readFile source
    doc <- liftIO $ runIO $ readMarkdown readerSettings src
    doc' <- liftIO $ handleError doc
    let title = getPandocTitle doc'
    innerHtml <- liftIO $ runIO $ do
      cited <- applyFilters readerSettings [CiteprocFilter] [] doc'
      writeHtml5String def cited
    innerHtml' <- liftIO $ handleError innerHtml
    let templated = LT.toStrict $ renderText $ makePage title innerHtml'
    liftIO $ TIO.writeFile out templated



-- | Pandoc settings. We're using YAML metadata blocks,
--   Smart quotes, and HTML5 <figure> tags for images.
readerSettings :: ReaderOptions
readerSettings =
  def
    { readerExtensions =
        extensionsFromList
          [ Ext_auto_identifiers
          , Ext_citations
          , Ext_fenced_code_blocks
          , Ext_footnotes
          , Ext_implicit_figures
          , Ext_link_attributes
          , Ext_raw_html
          , Ext_simple_tables
          , Ext_smart
          , Ext_yaml_metadata_block
          ]
    }

writerSettings :: WriterOptions
writerSettings =
  def
    { writerExtensions =
        extensionsFromList
          [ Ext_auto_identifiers
          , Ext_citations
          , Ext_fenced_code_blocks
          , Ext_footnotes
          , Ext_implicit_figures
          , Ext_link_attributes
          , Ext_raw_html
          , Ext_simple_tables
          , Ext_smart
          , Ext_yaml_metadata_block
          ]
    }

-- | Extract the title from a Pandoc doc
getPandocTitle :: Pandoc -> T.Text
getPandocTitle doc = do
  let meta = case doc of
               Pandoc m _ -> m -- extract metadata
  stringify $ docTitle meta


-- | Main template, scaffolds any page, given its parent heading
--   ("index" if at the top level).
makePage :: T.Text -> T.Text -> Html ()
makePage title content = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8" ]
      title_ $ toHtml title
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      meta_ [ name_ "author", content_ "" ]
      meta_ [ name_ "description", content_ "" ]
      meta_ [ name_ "keywords", content_ "" ]
      mapM (\uri -> link_ [ rel_ "stylesheet", href_ uri ]) [ "/static/tufte-css/tufte.min.css"
                                                            , "/static/tufte-css/latex.css"
                                                            -- , "https://fonts.googleapis.com/css?family=Baumans"
                                                            ]
    body_ $ do
      article_ $ do
        h1_ [] $ toHtml title
        section_ [] $ toHtmlRaw content
      footer_ [] $ do
        "Coda goes here. Coda stuff here."
        script ""


script :: T.Text -> Html ()
script uri = script_ [ src_ uri ] (""::T.Text)


-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root =
  defaultSettings
    -- Disable directory listings
    { ssListing = Nothing }
  where
    defaultSettings = defaultFileServerSettings root

-- | Run a HTTP server to serve a directory of static files
serve ::
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve port path = do
  putStrLn $ "Serving at http://localhost:" <> show port
  Warp.run port $ staticApp $ staticSiteServerSettings path
