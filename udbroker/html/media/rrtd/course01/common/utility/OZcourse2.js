<!--
function WriteSWF(flName, flColor, flWidth, flHeight, strParameter){

document.write('<OBJECT '
      +  'classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"'
      + ' codebase="http://download.macromedia.com/pub/shockwave/'
      + 'cabs/flash/swflash.cab#version=6,0,0,0"'
      + ' ID="PositionIndicator"'
      + ' WIDTH=' + flWidth
      + ' HEIGHT=' + flHeight + '>'
      + '<PARAM NAME=movie VALUE="' + flName + strParameter + '">'
      + '<PARAM NAME=FlashVars VALUE="scoNumber='+strParameter+'&checklink=OZtime">'
      + '<PARAM NAME=quality VALUE=high>'
      + '<param name=menu value=false>'
	  + '<PARAM NAME=wmode VALUE=transparent>'
      + '<PARAM NAME=bgcolor VALUE=' + flColor + '>'
      + '<EMBED src="' + flName + strParameter + '"'
      + ' name="PositionIndicator"'
	  + ' FlashVars="scoNumber='+strParameter+'"'
      + ' quality=high menu=false wmode=transparent bgcolor=' + flColor
      + ' WIDTH=' + flWidth
      + ' HEIGHT=' + flHeight
      + ' TYPE="application/x-shockwave-flash"'
      + ' PLUGINSPAGE="http://www.macromedia.com/go/getflashplayer">'
      + '</EMBED></OBJECT>'
      )

}
//-->