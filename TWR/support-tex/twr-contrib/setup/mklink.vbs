'
' One argument is needed: 
' 1) path to the shell script with and initial configuration of support tools 
If WScript.Arguments.Count >= 1 Then
	' WScript.Echo  WScript.Arguments.Item(0)
	Set oShell = CreateObject("WScript.Shell")
	Set objArgs = WScript.Arguments
	Set oShortCut = oShell.CreateShortcut("MTM-FHM support tools.lnk")
	' Ger location of 
	sComSpec = oShell.ExpandEnvironmentStrings("%ComSpec%")
	oShortCut.TargetPath = sComSpec 
	oShortCut.Arguments = " /K " & objArgs.Item(0)
	oShortCut.Save
End If
