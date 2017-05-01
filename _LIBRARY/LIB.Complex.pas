unit LIB.Complex;

interface //#################################################################### ■

uses LIB;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDoubleC

     TDoubleC = record
     private
       ///// アクセス
       function GetAbs2 :Double;
       function GetAbso :Double;
     public
       R :Double;
       I :Double;
       /////
       constructor Create( const R_,I_:Double );
       ///// プロパティ
       property Abs2 :Double read GetAbs2;
       property Abso :Double read GetAbso;
       ///// 演算子
       class operator Negative( const V_:TDoubleC ) :TDoubleC;
       class operator Positive( const V_:TDoubleC ) :TDoubleC;
       class operator Add( const A_,B_:TDoubleC ) :TDoubleC;
       class operator Subtract( const A_,B_:TDoubleC ) :TDoubleC;
       class operator Multiply( const A_,B_:TDoubleC ) :TDoubleC;
       class operator Multiply( const A_:TDoubleC; const B_:Double ) :TDoubleC;
       class operator Multiply( const A_:Double; const B_:TDoubleC ) :TDoubleC;
       class operator Divide( const A_,B_:TDoubleC ) :TDoubleC;
       class operator Divide( const A_:TDoubleC; const B_:Double ) :TDoubleC;
       ///// 型変換
       class operator Implicit( const V_:Double ) :TDoubleC;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDoubleAreaC

     TDoubleAreaC = record
     private
       ///// アクセス
       function GetCentR :Double;
       procedure SetCentR( const CentR_:Double );
       function GetCentI :Double;
       procedure SetCentI( const CentI_:Double );
       function GetCent :TDoubleC;
       procedure SetCent( const Cent_:TDoubleC );
       function GetSizeR :Double;
       procedure SetSizeR( const SizeR_:Double );
       function GetSizeI :Double;
       procedure SetSizeI( const SizeI_:Double );
       function GetSize :TDoubleC;
       procedure SetSize( const Size_:TDoubleC );
     public
       Min :TDoubleC;
       Max :TDoubleC;
       /////
       constructor Create( const Min_,Max_:TDoubleC ); overload;
       constructor Create( const MinR_,MinI_,MaxR_,MaxI_:Double ); overload;
       constructor Create( const Cent_:TDoubleC; const SizeR_,SizeI_:Double ); overload;
       ///// プロパティ
       property CentR :Double   read GetCentR write SetCentR;
       property CentI :Double   read GetCentI write SetCentI;
       property Cent  :TDoubleC read GetCent  write SetCent ;
       property SizeR :Double   read GetSizeR write SetSizeR;
       property SizeI :Double   read GetSizeI write SetSizeI;
       property Size  :TDoubleC read GetSize  write SetSize ;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function Pow2( const X_:TDoubleC ) :TDoubleC; inline; overload;

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDoubleC

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TDoubleC.GetAbs2 :Double;
begin
     Result := Pow2( R ) + Pow2( I );
end;

function TDoubleC.GetAbso :Double;
begin
     Result := Roo2( GetAbs2 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDoubleC.Create( const R_,I_:Double );
begin
     R := R_;
     I := I_;
end;

///////////////////////////////////////////////////////////////////////// 演算子


class operator TDoubleC.Negative( const V_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := -V_.R;
          I := -V_.I;
     end
end;

class operator TDoubleC.Positive( const V_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := +V_.R;
          I := +V_.I;
     end
end;

class operator TDoubleC.Add( const A_,B_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := A_.R + B_.R;
          I := A_.I + B_.I;
     end
end;

class operator TDoubleC.Subtract( const A_,B_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := A_.R - B_.R;
          I := A_.I - B_.I;
     end
end;

class operator TDoubleC.Multiply( const A_,B_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := A_.R * B_.R - A_.I * B_.I;
          I := A_.R * B_.I + A_.I * B_.R;
     end
end;

class operator TDoubleC.Multiply( const A_:TDoubleC; const B_:Double ) :TDoubleC;
begin
     with Result do
     begin
          R := A_.R * B_;
          I := A_.I * B_;
     end
end;

class operator TDoubleC.Multiply( const A_:Double; const B_:TDoubleC ) :TDoubleC;
begin
     with Result do
     begin
          R := A_ * B_.R;
          I := A_ * B_.I;
     end
end;

class operator TDoubleC.Divide( const A_,B_:TDoubleC ) :TDoubleC;
var
   C :Double;
begin
     C := B_.Abs2;

     with Result do
     begin
          R := ( A_.R * B_.R + A_.I * B_.I ) / C;
          I := ( A_.I * B_.R - A_.R * B_.I ) / C;
     end
end;

class operator TDoubleC.Divide( const A_:TDoubleC; const B_:Double ) :TDoubleC;
begin
     with Result do
     begin
          R := A_.R / B_;
          I := A_.I / B_;
     end
end;

///////////////////////////////////////////////////////////////////////// 型変換

class operator TDoubleC.Implicit( const V_:Double ) :TDoubleC;
begin
     with Result do
     begin
          R := V_;
          I := 0;
     end
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDoubleAreaC

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TDoubleAreaC.GetCentR :Double;
begin
     Result := ( Max.R + Min.R ) / 2;
end;

procedure TDoubleAreaC.SetCentR( const CentR_:Double );
var
   S :Double;
begin
     S := SizeR / 2;

     Min.R := CentR_ - S;
     Max.R := CentR_ + S;
end;

function TDoubleAreaC.GetCentI :Double;
begin
     Result := ( Max.I + Min.I ) / 2;
end;

procedure TDoubleAreaC.SetCentI( const CentI_:Double );
var
   S :Double;
begin
     S := SizeI / 2;

     Min.I := CentI_ - S;
     Max.I := CentI_ + S;
end;

function TDoubleAreaC.GetCent :TDoubleC;
begin
     Result := TDoubleC.Create( CentR, CentI );
end;

procedure TDoubleAreaC.SetCent( const Cent_:TDoubleC );
begin
     CentR := Cent_.R;
     CentI := Cent_.I;
end;

//------------------------------------------------------------------------------

function TDoubleAreaC.GetSizeR :Double;
begin
     Result := Max.R - Min.R;
end;

procedure TDoubleAreaC.SetSizeR( const SizeR_:Double );
var
   C, S :Double;
begin
     C := CentR;
     S := SizeR_ / 2;

     Min.R := C - S;
     Max.R := C + S;
end;

function TDoubleAreaC.GetSizeI :Double;
begin
     Result := Max.I - Min.I;
end;

procedure TDoubleAreaC.SetSizeI( const SizeI_:Double );
var
   C, S :Double;
begin
     C := CentI;
     S := SizeI_ / 2;

     Min.I := C - S;
     Max.I := C + S;
end;

function TDoubleAreaC.GetSize :TDoubleC;
begin
     Result := TDoubleC.Create( SizeR, SizeI );
end;

procedure TDoubleAreaC.SetSize( const Size_:TDoubleC );
begin
     SizeR := Size_.R;
     SizeI := Size_.I;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDoubleAreaC.Create( const Min_,Max_:TDoubleC );
begin
     Min := Min_;
     Max := Max_;
end;

constructor TDoubleAreaC.Create( const MinR_,MinI_,MaxR_,MaxI_:Double );
begin
     Create( TDoubleC.Create( MinR_, MinI_ ),
             TDoubleC.Create( MaxR_, MaxI_ ) );
end;

constructor TDoubleAreaC.Create( const Cent_:TDoubleC; const SizeR_,SizeI_:Double );
var
   S :TDoubleC;
begin
     S.R := SizeR_ / 2;
     S.I := SizeI_ / 2;

     Create( Cent_ - S,
             Cent_ + S );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function Pow2( const X_:TDoubleC ) :TDoubleC;
begin
     Result := X_ * X_;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
