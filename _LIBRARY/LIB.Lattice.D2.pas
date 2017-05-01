unit LIB.Lattice.D2;

interface //#################################################################### ■

uses System.UITypes,
     FMX.Graphics,
     LIB;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMap2D<_TYPE_>

     TMap2D<_TYPE_> = class
     private
       ///// メソッド
       procedure InitMap;
     protected
       _Map   :TArray2<_TYPE_>;
       _SizeX :Integer;
       _SizeY :Integer;
       ///// アクセス
       procedure SetSizeX( const SizeX_:Integer );
       procedure SetSizeY( const SizeY_:Integer );
     public
       constructor Create; overload;
       constructor Create( const SizeX_,SizeY_:Integer ); overload; virtual;
       ///// プロパティ
       property SizeX :Integer read _SizeX write SetSizeX;
       property SizeY :Integer read _SizeY write SetSizeY;
       ///// メソッド
       procedure SetSize( const SizeX_,SizeY_:Integer );
       procedure Clear( const Value_:_TYPE_ );
       procedure LoadFromFile( const FileName_:String ); virtual; abstract;
       procedure SaveToFile( const FileName_:String ); virtual; abstract;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TColorMap2D

     TColorMap2D = class( TMap2D<TAlphaColorF> )
     private
     protected
     public
       ///// メソッド
       procedure LoadFromFile( const FileName_:String ); override;
       procedure SaveToFile( const FileName_:String ); override;
       procedure ImportFrom( const BMP_:TBitmap );
       procedure ExportTo( const BMP_:TBitmap );
       function Interp( const X_,Y_:Single ) :TAlphaColorF;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math, System.Threading, System.Classes;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMap2D<_TYPE_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMap2D<_TYPE_>.InitMap;
begin
     SetLength( _Map, _SizeY, _SizeX );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TMap2D<_TYPE_>.SetSizeX( const SizeX_:Integer );
begin
     _SizeX := SizeX_;

     InitMap;
end;

procedure TMap2D<_TYPE_>.SetSizeY( const SizeY_:Integer );
begin
     _SizeY := SizeY_;

     InitMap;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMap2D<_TYPE_>.Create;
begin
     Create( 0, 0 );
end;

constructor TMap2D<_TYPE_>.Create( const SizeX_,SizeY_:Integer );
begin
     inherited Create;

     SetSize( SizeX_, SizeY_ );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMap2D<_TYPE_>.SetSize( const SizeX_,SizeY_:Integer );
begin
     _SizeX := SizeX_;
     _SizeY := SizeY_;

     InitMap;
end;

//------------------------------------------------------------------------------

procedure TMap2D<_TYPE_>.Clear( const Value_:_TYPE_ );
var
   X, Y :Integer;
begin
     for Y := 0 to _SizeY-1 do
     begin
          for X := 0 to _SizeX-1 do _Map[ Y, X ] := Value_;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TColorMap2D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TColorMap2D.LoadFromFile( const FileName_:String );
var
   B :TBitmap;
begin
     B := TBitmap.Create;

     B.LoadFromFile( FileName_ );

     ImportFrom( B );

     B.DisposeOf;
end;

procedure TColorMap2D.SaveToFile( const FileName_:String );
var
   B :TBitmap;
begin
     B := TBitmap.Create;

     ExportTo( B );

     B.SaveToFile( FileName_ );

     B.DisposeOf;
end;

//------------------------------------------------------------------------------

procedure TColorMap2D.ImportFrom( const BMP_:TBitmap );
var
   B :TBitmapData;
   X, Y :Integer;
   P :PAlphaColor;
begin
     SetSize( BMP_.Width, BMP_.Height );

     BMP_.Map( TMapAccess.Read, B );

     for Y := 0 to _SizeY-1 do
     begin
          P := B.GetScanline( Y );

          for X := 0 to _SizeX-1 do
          begin
               _Map[ Y, X ] := TAlphaColorF.Create( P^ );  Inc( P );
          end;
     end;

     BMP_.Unmap( B );
end;

procedure TColorMap2D.ExportTo( const BMP_:TBitmap );
var
   B :TBitmapData;
   X, Y :Integer;
   P :PAlphaColor;
begin
     BMP_.SetSize( _SizeX, _SizeY );

     BMP_.Map( TMapAccess.Write, B );

     for Y := 0 to _SizeY-1 do
     begin
          P := B.GetScanline( Y );

          for X := 0 to _SizeX-1 do
          begin
               P^ := _Map[ Y, X ].ToAlphaColor;  Inc( P );
          end;
     end;

     BMP_.Unmap( B );
end;

//------------------------------------------------------------------------------

function TColorMap2D.Interp( const X_,Y_:Single ) :TAlphaColorF;
var
   Xi, Yi :Integer;
begin
     Xi := Clamp( Floor( _SizeX * X_ ), 0, _SizeX-1 );
     Yi := Clamp( Floor( _SizeY * Y_ ), 0, _SizeY-1 );

     Result := _Map[ Xi, Yi ];
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■