<!--
function WriteSWF(flName, flColor, flWidth, flHeight, cookString,check_link){
	//var outString;
      // Name the variables
      //flName = "flashes/scoa_t01.swf"
      //flColor = "#ffffff"
      //flHeight = "300"
      //flWidth = "500"
	  //cookString = "?AudioStatus=" + parent.parent.parent.AudioStatus + "&LongBook=" + parent.parent.parent.LongBook
      // This portion of the script is a modified version of Colin Moock's
      // flash cookie importer, query string version script.
      // Slight modifications have been made to
      // work with the get cookies script.
document.write('<OBJECT '
      +  'classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"'
      + ' codebase="http://active.macromedia.com/flash2/'
      + 'cabs/swflash.cab#version=6,0,0,0"'
      + ' ID="navigator"'
      + ' WIDTH=' + flWidth
      + ' HEIGHT=' + flHeight + '>'
      + '<PARAM NAME=movie VALUE="' + flName + cookString + '">'
      + '<PARAM NAME=FlashVars VALUE="'+locationStr+'">'
//<PARAM NAME=FlashVars VALUE="foo=Hello%20World&paragraph=first+line%0Dsecond+line">
      + '<PARAM NAME=quality VALUE=high>'
      + '<PARAM NAME=bgcolor VALUE=' + flColor + '>'
      + '<EMBED src="' + flName + cookString +'"'
      + ' name="navigator"'
      + ' quality=high bgcolor=' + flColor
      + ' WIDTH=' + flWidth
      + ' HEIGHT=' + flHeight
      + ' TYPE="application/x-shockwave-flash"'
      + ' PLUGINSPAGE="http://www.macromedia.com/shockwave/'
      + 'download/index.cgi?P1_Prod_Version=ShockwaveFlash">'
      + '</EMBED></OBJECT>'
      )

}
//-->