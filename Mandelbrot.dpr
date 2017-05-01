program Mandelbrot;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LIB in '_LIBRARY\LIB.pas',
  LIB.Complex in '_LIBRARY\LIB.Complex.pas',
  LIB.Lattice.D1 in '_LIBRARY\LIB.Lattice.D1.pas',
  LIB.Lattice.D2 in '_LIBRARY\LIB.Lattice.D2.pas',
  LIB.Render in '_LIBRARY\LIB.Render.pas',
  LIB.Fractal in '_LIBRARY\LIB.Fractal.pas',
  LIB.Fractal.Mandelbrot in '_LIBRARY\LIB.Fractal.Mandelbrot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
