document.getElementById('thisweek').onclick = function () {
  var today = new Date()
  var todayISO = today.toISOString().substr(0,10)
  var h2s = document.getElementsByTagName('h2')
  for (let h2 of h2s) {
    var h2ID = h2.id.match(/2020-\d\d-\d\d/)
    // console.log(h2ID)
    if (h2ID > todayISO) {
      console.log(h2ID)
      console.log(h2)
      h2.scrollIntoView()
      window.location = '#' + h2.id
      break
    }
  }
}
