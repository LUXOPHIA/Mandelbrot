unit LIB.Lattice.D1;

interface //#################################################################### ■

uses System.UITypes,
     FMX.Graphics,
     LIB;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMap1D<_TYPE_>

     TMap1D<_TYPE_> = class
     private
       ///// メソッド
       procedure InitMap;
     protected
       _Map   :TArray<_TYPE_>;
       _SizeX :Integer;
       ///// アクセス
       procedure SetSizeX( const SizeX_:Integer );
     public
       constructor Create; overload;
       constructor Create( const SizeX_:Integer ); overload; virtual;
       ///// プロパティ
       property SizeX :Integer read _SizeX write SetSizeX;
       ///// メソッド
       procedure Clear( const Value_:_TYPE_ );
       procedure LoadFromFile( const FileName_:String ); virtual; abstract;
       procedure SaveToFile( const FileName_:String ); virtual; abstract;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TColorMap1D

     TColorMap1D = class( TMap1D<TAlphaColorF> )
     private
     protected
     public
       ///// メソッド
       procedure LoadFromFile( const FileName_:String ); override;
       procedure SaveToFile( const FileName_:String ); override;
       procedure ImportFrom( const BMP_:TBitmap );
       procedure ExportTo( const BMP_:TBitmap );
       function Interp( const X_:Single ) :TAlphaColorF;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMap1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMap1D<_TYPE_>.InitMap;
begin
     SetLength( _Map, _SizeX );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TMap1D<_TYPE_>.SetSizeX( const SizeX_:Integer );
begin
     _SizeX := SizeX_;

     InitMap;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMap1D<_TYPE_>.Create;
begin
     Create( 0 );
end;

constructor TMap1D<_TYPE_>.Create( const SizeX_:Integer );
begin
     inherited Create;

     _SizeX := SizeX_;

     InitMap;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMap1D<_TYPE_>.Clear( const Value_:_TYPE_ );
var
   X :Integer;
begin
     for X := 0 to _SizeX-1 do _Map[ X ] := Value_;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TColorMap1D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TColorMap1D.LoadFromFile( const FileName_:String );
var
   B :TBitmap;
begin
     B := TBitmap.Create;

     B.LoadFromFile( FileName_ );

     ImportFrom( B );

     B.DisposeOf;
end;

procedure TColorMap1D.SaveToFile( const FileName_:String );
var
   B :TBitmap;
begin
     B := TBitmap.Create;

     ExportTo( B );

     B.SaveToFile( FileName_ );

     B.DisposeOf;
end;

//------------------------------------------------------------------------------

procedure TColorMap1D.ImportFrom( const BMP_:TBitmap );
var
   B :TBitmapData;
   X :Integer;
   P :PAlphaColor;
begin
     SizeX := BMP_.Width;

     BMP_.Map( TMapAccess.Read, B );

     P := B.GetScanline( 0 );

     for X := 0 to _SizeX-1 do
     begin
          _Map[ X ] := TAlphaColorF.Create( P^ );  Inc( P );
     end;

     BMP_.Unmap( B );
end;

procedure TColorMap1D.ExportTo( const BMP_:TBitmap );
var
   B :TBitmapData;
   X :Integer;
   P :PAlphaColor;
begin
     BMP_.SetSize( SizeX, 1 );

     BMP_.Map( TMapAccess.Write, B );

     P := B.GetScanline( 0 );

     for X := 0 to _SizeX-1 do
     begin
          P^ := _Map[ X ].ToAlphaColor;  Inc( P );
     end;

     BMP_.Unmap( B );
end;

//------------------------------------------------------------------------------

function TColorMap1D.Interp( const X_:Single ) :TAlphaColorF;
var
   Xi :Integer;
begin
     Xi := Clamp( Floor( _SizeX * X_ ), 0, _SizeX-1 );

     Result := _Map[ Xi ];
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■