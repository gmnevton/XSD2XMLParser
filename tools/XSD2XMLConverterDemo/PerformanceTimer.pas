unit PerformanceTimer;

interface

uses
  SysUtils,
  Windows,
  DateUtils;

type
  TPerformanceTimer = class
  private
    FFrequency: TLargeInteger;
    FIsRunning: boolean;
    FIsHighResolution: boolean;
    FStartCount, FStopCount: TLargeInteger;
    procedure SetTickStamp(var Value: TLargeInteger);
    function GetElapsedTicks: TLargeInteger;
    function GetElapsedMiliseconds: TLargeInteger;
    function GetElapsedMicroseconds: TLargeInteger;
  public
    constructor Create(const StartOnCreate: Boolean = False);
    procedure Start;
    procedure Stop;
    property IsHighResolution: Boolean read FIsHighResolution;
    property ElapsedTicks: TLargeInteger read GetElapsedTicks;
    property ElapsedMiliseconds : TLargeInteger read GetElapsedMiliseconds;
    property ElapsedMicroseconds: TLargeInteger read GetElapsedMicroseconds;
    property IsRunning: Boolean read FIsRunning;
end;

implementation

constructor TPerformanceTimer.Create(const StartOnCreate: Boolean = False);
begin
  inherited Create;
  FIsRunning:=False;

  FIsHighResolution:=QueryPerformanceFrequency(FFrequency);
  if not FIsHighResolution then
    FFrequency:=MSecsPerSec;

  if StartOnCreate then
    Start;
end;

procedure TPerformanceTimer.SetTickStamp(var Value: TLargeInteger);
begin
  if FIsHighResolution then
    QueryPerformanceCounter(Value)
  else
    Value:=MilliSecondOf(Now);
end;

function TPerformanceTimer.GetElapsedTicks: TLargeInteger;
begin
  Result:=FStopCount - FStartCount;
end;

function TPerformanceTimer.GetElapsedMiliseconds: TLargeInteger;
begin
  Result:=MSecsPerSec * GetElapsedTicks div FFrequency;
end;

function TPerformanceTimer.GetElapsedMicroseconds: TLargeInteger;
begin
  Result:=MSecsPerSec * MSecsPerSec * GetElapsedTicks div FFrequency;
end;

procedure TPerformanceTimer.Start;
begin
  SetTickStamp(FStartCount);
  FIsRunning:=True;
end;

procedure TPerformanceTimer.Stop;
begin
  SetTickStamp(FStopCount);
  FIsRunning:=False;
end;

end.
