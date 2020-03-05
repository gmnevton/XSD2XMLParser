program XSD2XMLConverterDemo;

uses
  Forms,
  uMain in 'uMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
//  ReportMemoryLeaksOnShutdown:=True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
