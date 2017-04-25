unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Diagnostics, System.Threading,
  FMX.StdCtrls, FMX.Edit, FMX.ExtCtrls, FMX.Controls.Presentation, FMX.Objects,
  LIB, LIB.Complex;

type
  TForm1 = class(TForm)
    Image1: TImage;
    ProgressBar1: TProgressBar;
    GroupBoxI: TGroupBox;
    LabelW: TLabel;
    EditW: TEdit;
    LabelH: TLabel;
    EditH: TEdit;
    LabelA: TLabel;
    PopupBoxA: TPopupBox;
    LabelN: TLabel;
    EditN: TEdit;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    GroupBoxR: TGroupBox;
    ButtonP: TButton;
    ButtonB: TButton;
    LabelT: TLabel;
    LabelTu: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Resize(Sender: TObject);
    procedure EditWValidate(Sender: TObject; var Text: string);
    procedure EditHValidate(Sender: TObject; var Text: string);
    procedure PopupBoxAChange(Sender: TObject);
    procedure EditNValidate(Sender: TObject; var Text: string);
    procedure ButtonPClick(Sender: TObject);
    procedure ButtonBClick(Sender: TObject);
  private
    { private 宣言 }
    _Clock :TStopwatch;
    _Image :TBitmapData;
    _Task  :ITask;
    ///// メソッド
    function ScreenToComplex( const X_,Y_:Integer ) :TDoubleC;
    function ComplexToColor( const C_:TDoubleC ) :TAlphaColorF;
  public
    { public 宣言 }
    _SizeW :Integer;
    _SizeH :Integer;
    _FuncN :Integer;
    _AreaC :TDoubleAreaC;
    ///// メソッド
    procedure BeginRender;
    procedure EndRender;
    procedure RenderS;
    procedure RenderT;
    procedure RenderP;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

function TForm1.ScreenToComplex( const X_,Y_:Integer ) :TDoubleC;
begin
     with _AreaC do
     begin
          Result.R := Min.R + SizeR / _SizeW * ( X_ + 0.5 );
          Result.I := Max.I - SizeI / _SizeH * ( Y_ + 0.5 );
     end;
end;

function TForm1.ComplexToColor( const C_:TDoubleC ) :TAlphaColorF;
const
     C0 :TAlphaColorF = ( R:0; G:0; B:0; A:1 );
     C1 :TAlphaColorF = ( R:1; G:1; B:1; A:1 );
var
   Z :TDoubleC;
   N :Integer;
begin
     Z := 0;

     for N := 1 to _FuncN do
     begin
          Z := Pow2( Z ) + C_;

          if Z.Abso > 2 then
          begin
               Result := ( C1 - C0 ) * N / _FuncN + C0;

               Exit;
          end;
     end;

     Result := C1;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.BeginRender;
begin
     ButtonP.Enabled := False;
     ButtonB.Enabled := True;

     with Image1.Bitmap do
     begin
          SetSize( _SizeW, _SizeH );

          Clear( TAlphaColors.Null );

          Map( TMapAccess.Write, _Image );
     end;

     with ProgressBar1 do
     begin
          Max     := _SizeH;
          Value   := 0;
          Visible := True;
     end;

     _Clock := TStopWatch.StartNew;
end;

procedure TForm1.EndRender;
begin
     _Clock.Stop;

     ProgressBar1.Visible := False;

     _Task := nil;

     with Image1.Bitmap do
     begin
          Unmap( _Image );

          if ButtonB.Enabled then SaveToFile( 'Image.png' );
     end;

     LabelT.Text := _Clock.Elapsed.TotalSeconds.ToString;

     ButtonP.Enabled := True;
     ButtonB.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TForm1.RenderS;
var
   X, Y :Integer;
   C :TDoubleC;
begin
     BeginRender;

     for Y := 0 to _SizeH-1 do
     begin
          for X := 0 to _SizeW-1 do
          begin
               C := ScreenToComplex( X, Y );

               _Image.Pixels[ X, Y ] := ComplexToColor( C ).ToAlphaColor;
          end;

          ProgressBar1.Value := Y;

          Application.ProcessMessages;

          if not ButtonB.Enabled then Break;
     end;

     EndRender;
end;

procedure TForm1.RenderT;
begin
     _Task := TTask.Create(
          procedure
          var
             Y, X :Integer;
             C :TDoubleC;
          begin
               TThread.Synchronize( nil, BeginRender );

               try
                  for Y := 0 to _SizeH-1 do
                  begin
                       TTask.CurrentTask.CheckCanceled;

                       for X := 0 to _SizeW-1 do
                       begin
                            C := ScreenToComplex( X, Y );

                            _Image.Pixels[ X, Y ] := ComplexToColor( C ).ToAlphaColor;
                       end;

                       TThread.Synchronize( nil,
                            procedure
                            begin
                                 ProgressBar1.Value := Y;
                            end );
                  end;

               finally
                      TThread.Synchronize( nil, EndRender );
               end;
          end );

     _Task.Start;
end;

procedure TForm1.RenderP;
begin
     _Task := TTask.Run(
          procedure
          begin
               TThread.Synchronize( nil, BeginRender );

               TParallel.For( 0, _SizeH-1,
                    procedure( Y:Integer; S:TParallel.TLoopState )
                    var
                       X :Integer;
                       C :TDoubleC;
                    begin
                         if _Task.Status = TTaskStatus.Running then
                         begin
                              for X := 0 to _SizeW-1 do
                              begin
                                   C := ScreenToComplex( X, Y );

                                   _Image.Pixels[ X, Y ] := ComplexToColor( C ).ToAlphaColor;
                              end;

                              TThread.Synchronize( nil,
                                   procedure
                                   begin
                                        with ProgressBar1 do Value := Value + 1;
                                   end );
                         end
                         else S.Stop;
                    end );

               TThread.Synchronize( nil, EndRender );
          end );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     EditW    .Text      := '500';
     EditH    .Text      := '500';
     PopupBoxA.ItemIndex :=    0 ;
     EditN    .Text      := '500';
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
     CanClose := ButtonP.Enabled;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Image1Resize(Sender: TObject);
begin
     ProgressBar1.Width := Image1.Width / 2;
end;

//------------------------------------------------------------------------------

procedure TForm1.EditWValidate(Sender: TObject; var Text: string);
begin
     if not TryStrToInt( Text, _SizeW ) then Text := _SizeW.ToString;
end;

procedure TForm1.EditHValidate(Sender: TObject; var Text: string);
begin
     if not TryStrToInt( Text, _SizeH ) then Text := _SizeH.ToString;
end;

procedure TForm1.PopupBoxAChange(Sender: TObject);
begin
     case PopupBoxA.ItemIndex of
       0: _AreaC := TDoubleAreaC.Create( -2.00000, -2.00000, +2.00000, +2.00000 );
       1: _AreaC := TDoubleAreaC.Create( -0.97217, -0.25280, -0.96717, -0.24780 );
       2: _AreaC := TDoubleAreaC.Create( +0.26220, -0.00250, +0.26270, -0.00200 );
       3: _AreaC := TDoubleAreaC.Create( -1.25500, +0.02520, -1.25450, +0.02570 );
       4: _AreaC := TDoubleAreaC.Create( -0.33800, -0.61500, -0.33700, -0.61400 );
       5: _AreaC := TDoubleAreaC.Create( -1.25500, +0.02370, -1.25300, +0.02570 );
       6: _AreaC := TDoubleAreaC.Create( +0.26000, -0.00300, +0.26200, -0.00100 );
     end;
end;

procedure TForm1.EditNValidate(Sender: TObject; var Text: string);
begin
     if not TryStrToInt( Text, _FuncN ) then Text := _FuncN.ToString;
end;

//------------------------------------------------------------------------------

procedure TForm1.ButtonPClick(Sender: TObject);
begin
     if RadioButton1.IsChecked then RenderS
                               else
     if RadioButton2.IsChecked then RenderT
                               else
     if RadioButton3.IsChecked then RenderP;
end;

procedure TForm1.ButtonBClick(Sender: TObject);
begin
     ButtonB.Enabled := False;

     if Assigned( _Task ) then _Task.Cancel;
end;

end. //######################################################################### ■
