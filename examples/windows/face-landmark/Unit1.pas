unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils,
  System.ImageList, FMX.ImgList, FMX.ListBox, FaceLandmarkFMX;

type
  TForm1 = class(TForm)
    ImageMain: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

  public
    procedure LoadImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


const
  ModelsPath = '..\..\..\..\..\models\';

var
  FaceLandmarkFMX: TFaceLandmarkFMX;

procedure TForm1.LoadImage;
begin
{$IFDEF MSWINDOWS}
  if not FileExists(OpenDialog.FileName) then
    Exit;

  if ImageMain.MultiResBitmap.Count > 0 then
    ImageMain.MultiResBitmap[0].Free;

  ImageMain.MultiResBitmap.Add;

  if FileExists(OpenDialog.FileName) then
  begin
    if ImageList.Source[0].MultiResBitmap.Count > 0 then
      ImageList.Source[0].MultiResBitmap[0].Free;

    ImageList.Source[0].MultiResBitmap.Add;
    ImageList.Source[0].MultiResBitmap[0].Bitmap.LoadFromFile(OpenDialog.FileName);
  end;

  ImageMain.Bitmap.Assign(ImageList.Source[0].MultiResBitmap[0].Bitmap);

{$ENDIF MSWINDOWS}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadImage;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  U: DWORD;
  FFaceLandmarkData: TFaceLandmarkData;
begin
  LoadImage;

  FFaceLandmarkData := FaceLandmarkFMX.GetLandmarkData(ImageMain.Bitmap);

  ImageMain.Bitmap.Canvas.BeginScene;
  try
    ImageMain.Bitmap.Canvas.Fill.Color := TAlphaColorRec.White;
    ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.White;

    for U := 0 to FFaceLandmarkData.Count - 1 do
    begin
      ImageMain.Bitmap.Canvas.FillRect(
        RectF(
        FFaceLandmarkData.Points[U].X,
        FFaceLandmarkData.Points[U].Y,
        FFaceLandmarkData.Points[U].X + 5,
        FFaceLandmarkData.Points[U].Y + 5),
        0, 0, AllCorners, 1);
    end;

  finally
    ImageMain.Bitmap.Canvas.EndScene;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FaceLandmarkFMX := TFaceLandmarkFMX.Create(Self);
  FaceLandmarkFMX.LoadModel(ModelsPath + 'face_landmark.tflite', 8);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceLandmarkFMX.Destroy;
end;

end.
