unit LIB.Fractal;

interface //#################################################################### ■

uses System.UITypes, System.TimeSpan, System.Classes, System.Threading, System.Diagnostics,
     LIB, LIB.Complex, LIB.Lattice.D1, LIB.Render;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TFractal

     TFractal = class( TRender )
     private
     protected
       _AreaC :TDoubleAreaC;
       _IterN :Integer;
       _Diver :Double;
       _Color :TColorMap1D;
       ///// メソッド
       function PixelToComplex( const X_,Y_:Single ) :TDoubleC;
       function ComplexToIterN( const C_:TDoubleC ) :Single; virtual; abstract;
       function GetRender( const X_,Y_:Integer ) :TAlphaColorF; override;
     public
       constructor Create( const SizeX_,SizeY_:Integer; const Color_:TColorMap1D ); overload;
       constructor Create( const Fractal_:TFractal ); overload;
       ///// プロパティ
       property AreaC :TDoubleAreaC read _AreaC write _AreaC;
       property IterN :Integer      read _IterN write _IterN;
       property Diver :Double       read _Diver write _Diver;
       property Color :TColorMap1D  read _Color write _Color;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Math,
     FMX.Graphics;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TFractal

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TFractal.PixelToComplex( const X_,Y_:Single ) :TDoubleC;
begin
     with _AreaC do
     begin
          Result.R := Min.R + SizeR / _SizeX * ( X_ + 0.5 );
          Result.I := Max.I - SizeI / _SizeY * ( Y_ + 0.5 );
     end;
end;

function TFractal.GetRender( const X_,Y_:Integer ) :TAlphaColorF;
var
   C :TDoubleC;
   N :Single;
begin
     C := PixelToComplex( X_, Y_ );

     N := ComplexToIterN( C );

     Result := _Color.Interp( N / _IterN );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TFractal.Create( const SizeX_,SizeY_:Integer; const Color_:TColorMap1D );
begin
     inherited Create( SizeX_, SizeY_ );

     _AreaC := TDoubleAreaC.Create( -2, -2, +2, +2 );
     _IterN := 500;
     _Diver := 1000;
     _Color := Color_;
end;

constructor TFractal.Create( const Fractal_:TFractal );
begin
     Create( Fractal_.SizeX, Fractal_.SizeY, Fractal_.Color );

     _AreaC := Fractal_.AreaC;
     _IterN := Fractal_.IterN;
     _Diver := Fractal_.Diver;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■