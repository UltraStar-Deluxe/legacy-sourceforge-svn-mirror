�
 TMAIN 0h  TPF0TMainMainLeftITopLBorderStylebsDialogCaptionTest ZLPortIOClientHeight� ClientWidth`Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderScaledOnCreate
FormCreatePixelsPerInch`
TextHeight TLabellb1LeftTop1Width0HeightCaption
Port num :  TLabellb2LeftrTop1WidthBHeightCaptionData to write :  TLabellb3LeftTopWidth.HeightCaption	Data type  TLabelLb4LeftrTopRWidth HeightCaptionRead :  TEditePortLeft>Top-Width+HeightCharCaseecUpperCaseFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 	MaxLength
ParentFontTabOrderText378	OnKeyDownePortKeyDown
OnKeyPressePortKeyPressOnKeyUp
ePortKeyUp  TEditeDataLeft� Top-Width@HeightHintTo write press enterCharCaseecUpperCaseFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 
ParentFontTabOrderText00	OnKeyDowneDataKeyDown
OnKeyPressePortKeyPress  TEditeRDataLeft� TopMWidth@HeightHintReadCharCaseecUpperCaseFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 
ParentFontReadOnly	TabOrder  TButtonbtnExitLeftTopWidthMHeightCaptionExitTabOrderOnClickbtnExitClick  	TComboBox
coDataTypeLeft>TopWidthQHeightStylecsDropDownList
ItemHeightTabOrder OnChangecoDataTypeChangeItems.StringsByte WordDword    	TGroupBoxgb1LeftToplWidthZHeightfCaption HELP TabOrder TLabellb5LeftTopWidth@HeightACaption�All data in editboxes must be entered in hex.
You can use UP and DOWN arrow keys to increment or decrement values in editbox
When you change port number, the program automatically do readport. You can use ENTER key in editboxes to do writeport;WordWrap	  TLabelllbWWWLeftyTopQWidthPHeightCursorcrHandPointHint:Click to check for new version at http://www.specosoft.com	AlignmenttaCenterAutoSizeCaptionweb   updateColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclNavyFont.Height�	Font.NameArial
Font.StylefsBoldfsItalicfsUnderline ParentColor
ParentFontWordWrap	OnClickllbWWWClick   
TStatusBarsbBarLeft Top� Width`HeightPanels SimplePanel	  TButtonbtnWriteLeftTop+WidthMHeightCaption	WritePortTabOrderOnClickbtnWriteClick  TButtonbtnReadLeftTopMWidthMHeightCaptionReadPortTabOrderOnClickbtnReadClick  	TCheckBoxcbDirectLeft� TopWidth9HeightHint�Allow direct access for IO ports
If enabled, then you can use asm instructions (in,out) to access IO ports.
Else you must use portwrite(read)X functions in your applicationsCaptionDirectTabOrder	OnClickcbDirectClick   