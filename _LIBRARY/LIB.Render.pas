unit LIB.Render;

interface //#################################################################### ■

uses System.UITypes, System.TimeSpan, System.Classes, System.Threading, System.Diagnostics,
     LIB, LIB.Lattice.D2;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRender

     TRender = class( TColorMap2D )
     private
       _ProgreTask :ITask;
       _RenderTask :ITask;
     protected
       _Timer :TStopwatch;
       _IsRun :Boolean;
       ///// イベント
       _OnBeginRender :TThreadProcedure;
       _OnProgress    :TThreadProcedure;
       _OnEndRender   :TThreadProcedure;
       ///// アクセス
       function GetTimer :TTimeSpan;
       ///// メソッド
       function GetRender( const X_,Y_:Integer ) :TAlphaColorF; virtual; abstract;
     public
       constructor Create( const SizeX_,SizeY_:Integer ); overload; override;
       ///// プロパティ
       property Timer :TTimeSpan read GetTimer;
       property IsRun :Boolean   read   _IsRun;
       ///// イベント
       property OnBeginRender :TThreadProcedure read _OnBeginRender write _OnBeginRender;
       property OnProgress    :TThreadProcedure read _OnProgress    write _OnProgress   ;
       property OnEndRender   :TThreadProcedure read _OnEndRender   write _OnEndRender  ;
       ///// メソッド
       procedure Render;
       procedure Cancel;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRender

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRender.GetTimer :TTimeSpan;
begin
     Result := _Timer.Elapsed;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRender.Create( const SizeX_,SizeY_:Integer );
begin
     inherited;

     _Timer := TStopWatch.Create;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRender.Render;
begin
     _RenderTask := TTask.Create( procedure
     begin
          Clear( TAlphaColorF.Create( TAlphaColors.Null ) );

          _IsRun := True;

          if Assigned( _OnBeginRender ) then TThread.Synchronize( nil, _OnBeginRender );

          _ProgreTask.Start;

          _Timer.Start;

          TParallel.For( 0, _SizeY-1, procedure( Y:Integer; S:TParallel.TLoopState )
          var
             X :Integer;
          begin
               if _RenderTask.Status = TTaskStatus.Running then
               begin
                    for X := 0 to _SizeX-1 do _Map[ Y, X ] := GetRender( X, Y );
               end
               else S.Stop;
          end );

          _Timer.Stop;

          _ProgreTask.Cancel;
     end );

     _ProgreTask := TTask.Create( procedure
     begin
          repeat
                Sleep( 500{ms} );

                if Assigned( _OnProgress ) then TThread.Synchronize( nil, _OnProgress );

          until TTask.CurrentTask.Status = TTaskStatus.Canceled;

          _IsRun := False;

          if Assigned( _OnEndRender ) then TThread.Synchronize( nil, _OnEndRender );
     end );

     _RenderTask.Start;
end;

procedure TRender.Cancel;
begin
     _RenderTask.Cancel;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■