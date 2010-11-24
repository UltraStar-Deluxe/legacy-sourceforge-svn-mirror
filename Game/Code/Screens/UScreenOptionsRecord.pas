unit UScreenOptionsRecord;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes, UCaptureWDM, UWebCam;

type
  TScreenOptionsRecord = class(TMenu)
    private
      SelectSlideInput:       integer;
      SelectSlideChannelL:    integer;
      SelectSlideChannelR:    integer;

      IWebCamDevice:            TList;
      IWebCamMedia:             TList;

      WebCamPreviewOn:          boolean;

      SelectSlideWebCamOnOff:   integer;
      SelectSlideWebCamDevice:  integer;
      SelectSlideWebCamMedia:   integer;
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure UpdateWebCam;
      procedure UpdateCard;
  end;

const
  ID='ID_011';   //for help system
  
implementation

uses SysUtils, UGraphic, URecord, UHelp, ULog;

function TScreenOptionsRecord.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    WebCamPreviewOn := (Ini.EnableWebCam=1);

    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_Q:
        begin
          Result := false;
        end;
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          Ini.Save;
          Music.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if (SelInteraction = 7) or ((Length(IWebCamDevice)=0) and (SelInteraction = 5)) then
          begin
            Ini.Save;
            Music.PlayBack;
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then
          begin
            Music.PlayOption;
            InteractInc;
          end;
          if SelInteraction = 0 then UpdateCard;

          if (SelInteraction >= 4) and (SelInteraction <= 6) then
          begin
            Music.PlayOption;
            InteractInc;
            UpdateWebCam;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then
          begin
            Music.PlayOption;
            InteractDec;
          end;
          if SelInteraction = 0 then UpdateCard;

          if (SelInteraction >= 4) and (SelInteraction <= 6) then
          begin
            Music.PlayOption;
            InteractDec;
            UpdateWebCam;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsRecord.Create;
var
  SC:     integer;
  SCI:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsRecord);

  SetLength(ICard, Length(Recording.SoundCard));

  if (Length(Recording.SoundCard)>0) then
  begin
    for SC := 0 to High(Recording.SoundCard) do
      ICard[SC] := Recording.SoundCard[SC].Description;

    SetLength(IInput, Length(Recording.SoundCard[Ini.Card].Input));
    for SCI := 0 to High(Recording.SoundCard[Ini.Card].Input) do
      IInput[SCI] := Recording.SoundCard[Ini.Card].Input[SCI].Name;

    AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, Ini.Card, ICard);
    SelectSlideInput    := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput, Ini.CardList[0].Input, IInput);
    SelectSlideChannelL := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelL, Ini.CardList[0].ChannelL, IChannel);
    SelectSlideChannelR := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelR, Ini.CardList[0].ChannelR, IChannel);
  end;

  GetCapDevices(IWebCamDevice);
  if (Length(IWebCamDevice)=0) then
    Ini.EnableWebCam := 0;

  SelectSlideWebCamOnOff := AddSelectSlide(Theme.OptionsRecord.SelectSlideWebCamOnOff, Ini.EnableWebCam, IEnableWebCam);
  if (Length(IWebCamDevice)>0) then
  begin
    if (Length(IWebCamDevice)-1 < Ini.WebCamID) then
      Ini.WebCamID := 0;

    IWebCamMedia := ListMediaTypes(Ini.WebCamID);

    SelectSlideWebCamDevice := AddSelectSlide(Theme.OptionsRecord.SelectSlideWebCamDevice, Ini.WebCamID, IWebCamDevice);
    SelectSlideWebCamMedia  := AddSelectSlide(Theme.OptionsRecord.SelectSlideWebCamMedia, Ini.WebCamMediaID, IWebCamMedia);

    WebCamPreviewOn := (Ini.EnableWebCam=1);
  end else
    WebCamPreviewOn := false;
  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsRecord.onShow;
begin
  Interaction := 0;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsRecord)');

  GetCapDevices(IWebCamDevice);

  if (Length(IWebCamDevice)>0) then
  begin
    if (Length(IWebCamDevice)-1 < Ini.WebCamID) then
      Ini.WebCamID := 0;

    IWebCamMedia := ListMediaTypes(Ini.WebCamID);

    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideCard, SelectSlideWebCamDevice, IWebCamDevice, Ini.WebCamID);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideWebCamMedia, IWebCamMedia, Ini.WebCamMediaID);

    WebCamPreviewOn := (Ini.EnableWebCam=1);
  end else
    WebCamPreviewOn := false;

  WebCamPreviewOn := wStartWebCam;
end;

procedure TScreenOptionsRecord.UpdateCard;
var
  SC:     integer;
  SCI:    integer;
begin
  SC := Ini.Card;

  SetLength(IInput, Length(Recording.SoundCard[SC].Input));
  for SCI := 0 to High(Recording.SoundCard[SC].Input) do
  begin
    IInput[SCI] := Recording.SoundCard[SC].Input[SCI].Name;
  end;

  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideInput, IInput, Ini.CardList[SC].Input);
  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelL, SelectSlideChannelL, IChannel, Ini.CardList[SC].ChannelL);
  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelR, SelectSlideChannelR, IChannel, Ini.CardList[SC].ChannelR);
end;

procedure TScreenOptionsRecord.onHide;
begin
  wClose;
end;

function TScreenOptionsRecord.Draw: boolean;
begin
  DrawBG;

  if WebCamPreviewOn then
  begin
    try
      wDraw(true, ScreenAct);
    except
      WebCamPreviewOn := false;
    end;
  end;

  DrawFG;
  Result := true;
end;

procedure TScreenOptionsRecord.UpdateWebCam;
begin
  wClose;

  GetCapDevices(IWebCamDevice);

  if (Length(IWebCamDevice)>0) then
  begin
    if (Length(IWebCamDevice)-1 < Ini.WebCamID) then
      Ini.WebCamID := 0;

    IWebCamMedia := ListMediaTypes(Ini.WebCamID);

    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideWebCamDevice, SelectSlideWebCamDevice, IWebCamDevice, Ini.WebCamID);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideWebCamMedia, SelectSlideWebCamMedia, IWebCamMedia, Ini.WebCamMediaID);

    WebCamPreviewOn := (Ini.EnableWebCam=1);
  end else
    WebCamPreviewOn := false;

  if WebCamPreviewOn then
    WebCamPreviewOn := wStartWebCam;
end;

end.