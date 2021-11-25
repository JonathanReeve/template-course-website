{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List (isInfixOf)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import qualified System.FilePath as SFP
import Lucid
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text.Encoding
-- Server stuff
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)
import Main.Utf8 (withUtf8)
import Data.Text
import Text.Pandoc.Citeproc (processCitations)

main :: IO ()
main = withUtf8 $
  shakeArgs shakeOptions{shakeFiles=".shake"} $ do

  let inputDir = "content"
  let outputDir = "dist"

  -- To serve the generated files (useful for previewing),
  -- run `shake serve`.
  phony "serve" $
    liftIO $ serve 8080 "dist/"

  -- Convert markdown to html
  action $ do
    srcs <- getDirectoryFiles "content" ["//*.md"]
    let neededFiles = ["dist" </> src -<.> "html" | src <- srcs]
    -- liftIO $ print neededFiles
    need neededFiles

  -- Copy over images and assets as-is
  action $ do
    srcs <- getDirectoryFiles "content" ["static//*"]
    need ["dist" </> src | src <- srcs]

  -- Copy over all static files
  "dist/static/**/*" %> \out -> do
    let source = "content" </> dropDirectory1 out
    need [source]
    copyFile' source out

  -- Convert HTML files from markdown
  "dist//*.html" %> \out -> do
    let source = "content" </> dropDirectory1 ( out -<.> "md" )
    need [source]
    src <- liftIO $ TIO.readFile source
    md <- liftIO $ md2html src
    let templated = renderText $ makePage md
    liftIO $ TIO.writeFile out templated



-- | Given a destination directory for an XML file, make a source directory
--   for it.
sourcify :: FilePath -> FilePath
sourcify dest = "src" </> dropThreeDirs (dest -<.> "xml")

-- | A utility function to strip two first directories from the beginning
--   of a file path.
dropTwoDirs :: FilePath -> FilePath
dropTwoDirs = dropDirectory1 . dropDirectory1

-- | For stripping three directories from the beginnings of file paths.
dropThreeDirs :: FilePath -> FilePath
dropThreeDirs = dropDirectory1 . dropDirectory1 . dropDirectory1

-- | Pandoc settings. We're using YAML metadata blocks,
--   Smart quotes, and HTML5 <figure> tags for images.
readerSettings :: ReaderOptions
readerSettings = def {readerExtensions = extensionsFromList [ Ext_yaml_metadata_block,
                            Ext_auto_identifiers, Ext_smart, Ext_implicit_figures, Ext_link_attributes, Ext_raw_html, Ext_footnotes, Ext_simple_tables ]}

filenameToUrl :: FilePath -> T.Text
filenameToUrl = T.pack

-- | Extract the title from a Pandoc doc
getPandocTitle :: Pandoc -> T.Text
getPandocTitle doc = do
  let meta = case doc of
               Pandoc m d -> m -- extract metadata
  stringify $ docTitle meta

-- Use Pandoc to convert markdown to html.
md2html :: T.Text -> IO Text
md2html mdDoc = do
  result <- runIO $ do
    doc <- readMarkdown readerSettings mdDoc
    cited <- processCitations doc
    writeHtml5String def cited
  handleError result

-- | Main template, scaffolds any page, given its parent heading
--   ("index" if at the top level).
makePage :: T.Text -> Html ()
makePage content = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8" ]
      title_ ""
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      meta_ [ name_ "author", content_ "" ]
      meta_ [ name_ "description", content_ "" ]
      meta_ [ name_ "keywords", content_ "" ]
      mapM (\uri -> link_ [ rel_ "stylesheet", href_ uri ]) [ "/includes/css/main.css"
                                                            , "https://fonts.googleapis.com/css?family=Baumans"
                                                            ]
    body_ $
      div_ $ toHtml content

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
