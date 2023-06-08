remark.macros.scale = function(w) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + w + '" />';
};

function playAudio(url) {
  var a = new Audio(url);
  a.play();
}
