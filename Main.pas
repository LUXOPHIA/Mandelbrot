unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Threading,
  FMX.StdCtrls, FMX.Edit, FMX.ExtCtrls, FMX.Controls.Presentation, FMX.Objects,
  LIB, LIB.Complex, LIB.Lattice.D1, LIB.Fractal.Mandelbrot;

type
  TForm1 = class(TForm)
    ImageV: TImage;
    ImageC: TImage;
    Panel1: TPanel;
      GroupBoxI: TGroupBox;
        GroupBoxIS: TGroupBox;
          LabelISX: TLabel;
            EditISX: TEdit;
          LabelISY: TLabel;
            EditISY: TEdit;
      GroupBoxF: TGroupBox;
        LabelFN: TLabel;
          EditFN: TEdit;
        LabelFD: TLabel;
          EditFD: TEdit;
      GroupBoxR: TGroupBox;
        ButtonRP: TButton;
        ButtonRB: TButton;
        LabelRT: TLabel;
          Label_RT: TLabel;
      GroupBoxA: TGroupBox;
        GroupBoxAC: TGroupBox;
          LabelACR: TLabel;
            EditACR: TEdit;
          LabelACI: TLabel;
            EditACI: TEdit;
      GroupBoxAS: TGroupBox;
        LabelASR: TLabel;
          EditASR: TEdit;
        LabelASI: TLabel;
          EditASI: TEdit;
    procedure Inputing( Sender_:TObject );
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ImageVPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ImageVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ImageVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ButtonRPClick(Sender: TObject);
    procedure ButtonRBClick(Sender: TObject);
  private
    { private 宣言 }
    _Mouse  :TShiftState;
    _DragP0 :TPointF;
    _DragP1 :TPointF;
    _DragA  :TDoubleAreaC;
    ///// アクセス
    procedure SetDragA( const DragA_:TDoubleAreaC );
  public
    { public 宣言 }
    _Color  :TColorMap1D;
    _Mande0 :TMandelbrot;
    _Mande1 :TMandelbrot;
    ///// プロパティ
    property DragA :TDoubleAreaC read _DragA write SetDragA;
    ///// メソッド
    function ViewToComp( const V_:TPointF ) :TDoubleC;
    function CompToView( const C_:TDoubleC ) :TPointF;
    function RectToArea( const R_:TRectF ) :TDoubleAreaC;
    function AreaToRect( const A_:TDoubleAreaC ) :TRectF;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

procedure TForm1.SetDragA( const DragA_:TDoubleAreaC );
begin
     _DragA := DragA_;  ImageV.Repaint;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TForm1.ViewToComp( const V_:TPointF ) :TDoubleC;
begin
     with _Mande0.AreaC do
     begin
          Result.R := Min.R + V_.X / ImageV.Width  * SizeR;
          Result.I := Max.I - V_.Y / ImageV.Height * SizeI;
     end;
end;

function TForm1.CompToView( const C_:TDoubleC ) :TPointF;
begin
     with _Mande0.AreaC do
     begin
          Result.X := ( C_ .R - Min.R ) / SizeR * ImageV.Width ;
          Result.Y := ( Max.I - C_ .I ) / SizeI * ImageV.Height;
     end;
end;

//------------------------------------------------------------------------------

function TForm1.RectToArea( const R_:TRectF ) :TDoubleAreaC;
var
   C0, C1 :TDoubleC;
begin
     C0 := ViewToComp( R_.TopLeft     );
     C1 := ViewToComp( R_.BottomRight );

     Result := TDoubleAreaC.Create( C0.R, C1.I, C1.R, C0.I );
end;

function TForm1.AreaToRect( const A_:TDoubleAreaC ) :TRectF;
var
   P0, P1 :TPointF;
begin
     P0 := CompToView( A_.Min );
     P1 := CompToView( A_.Max );

     Result := TRectF.Create( P0.X, P1.Y, P1.X, P0.Y );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.Inputing( Sender_:TObject );
begin
     if TMonitor.TryEnter( Self ) then
     begin
          TMonitor.Enter( Self );

          try
             with _Mande1 do
             begin
                  ///// 入力

                  if Sender_ = EditISX then SizeX       := EditISX.Text.ToInteger
                                       else
                  if Sender_ = EditISY then SizeY       := EditISY.Text.ToInteger
                                       else
                  if Sender_ = EditFN  then IterN       := EditFN .Text.ToInteger
                                       else
                  if Sender_ = EditFD  then Diver       := EditFD .Text.ToDouble
                                       else
                  if Sender_ = EditACR then AreaC.CentR := EditACR.Text.ToDouble
                                       else
                  if Sender_ = EditACI then AreaC.CentI := EditACI.Text.ToDouble
                                       else
                  if Sender_ = EditASR then AreaC.SizeR := EditASR.Text.ToDouble
                                       else
                  if Sender_ = EditASI then AreaC.SizeI := EditASI.Text.ToDouble
                                       else
                  if Sender_ = ImageV  then AreaC       := DragA;

                  ///// 表示

                  if Sender_ <> ImageV  then DragA := AreaC;

                  if Sender_ <> EditISX then EditISX.Text := SizeX.ToString;
                  if Sender_ <> EditISY then EditISY.Text := SizeY.ToString;

                  if Sender_ <> EditFN  then EditFN .Text := IterN.ToString;
                  if Sender_ <> EditFD  then EditFD .Text := Diver.ToString;

                  if Sender_ <> EditACR then EditACR.Text := FloatToStrF( AreaC.CentR, TFloatFormat.ffExponent, 7, 0 );
                  if Sender_ <> EditACI then EditACI.Text := FloatToStrF( AreaC.CentI, TFloatFormat.ffExponent, 7, 0 );
                  if Sender_ <> EditASR then EditASR.Text := FloatToStrF( AreaC.SizeR, TFloatFormat.ffExponent, 7, 0 );
                  if Sender_ <> EditASI then EditASI.Text := FloatToStrF( AreaC.SizeI, TFloatFormat.ffExponent, 7, 0 );

                  if Sender_ <> LabelRT then LabelRT.Text := _Mande0.Timer.TotalSeconds.ToString;

                  ///// 有効

                  ImageV   .Enabled := not IsRun;

                  GroupBoxI.Enabled := not IsRun;
                  GroupBoxF.Enabled := not IsRun;
                  GroupBoxA.Enabled := not IsRun;

                  ButtonRP .Enabled := not IsRun;
                  ButtonRB .Enabled :=     IsRun;
             end;

          except

          end;

          TMonitor.Exit( Self );
     end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
     ImageV.DisableInterpolation := True;
     ImageC.DisableInterpolation := True;

     _Mouse := [];

     _Color := TColorMap1D.Create;

     with _Color do
     begin
          LoadFromFile( '..\..\_DATA\Color.png' );
          ExportTo( ImageC.Bitmap );
     end;

     _Mande0 := TMandelbrot.Create;
     _Mande1 := TMandelbrot.Create( 512, 512, _Color );

     Inputing( Sender );

     ButtonRPClick( Sender );
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
     CanClose := not _Mande1.IsRun;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Mande0.DisposeOf;
     _Mande1.DisposeOf;

     _Color .DisposeOf;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.ImageVPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
     with Canvas do
     begin
          with Stroke do
          begin
               Color := TAlphaColors.Gray;
               Kind  := TBrushKind.Solid;
               Dash  := TStrokeDash.Dash;
          end;

          DrawRect( AreaToRect( _DragA ), 0, 0, [], 1 );
     end;
end;

//------------------------------------------------------------------------------

procedure TForm1.ImageVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _Mouse := Shift;

     _DragP0 := TPointF.Create( X, Y );
end;

procedure TForm1.ImageVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
   R :TRectF;
   V :TPointF;
begin
     if ssLeft in _Mouse then
     begin
          _DragP1 := TPointF.Create( X, Y );

          R := TRectF.Create( _DragP0 );

          V := _DragP1 - _DragP0;

          R.Inflate( Abs( V.X ), Abs( V.Y ) );

          _DragA := RectToArea( R );

          ImageV.Repaint;

          Inputing( ImageV );
     end;
end;

procedure TForm1.ImageVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     ImageVMouseMove( Sender, Shift, X, Y );

     _Mouse := [];
end;

//------------------------------------------------------------------------------

procedure TForm1.ButtonRPClick(Sender: TObject);
begin
     with _Mande1 do
     begin
          OnBeginRender := procedure
          begin
               Inputing( nil );
          end;

          OnProgress := procedure
          begin
               ExportTo( ImageV.Bitmap );
          end;

          OnEndRender := procedure
          begin
               ExportTo( ImageV.Bitmap );

               SaveToFile( 'Image.png' );

               _Mande0.DisposeOf;
               _Mande0 := _Mande1;
               _Mande1 := TMandelbrot.Create( _Mande0 );

               Inputing( nil );
          end;

          Render;
     end;
end;

procedure TForm1.ButtonRBClick(Sender: TObject);
begin
     _Mande1.Cancel;
end;

end. //######################################################################### ■
