PROGRAM upserver;
USES Classes, blcksock, sockets, Synautil, sysutils;

PROCEDURE AttendConnection(ASocket: TTCPBlockSocket);
VAR
  timeout: integer;
  s: ansistring;
  method, uri, protocol: string;
  OutputDataString: string;
  ResultCode: integer;
begin
  timeout := 120000;

  writeln('Received headers+document from browser:');

  s := ASocket.RecvString(timeout);
  writeln(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');

  repeat
    s := ASocket.RecvString(Timeout);
    writeln(s);
  until s = '';
  writeln('URI=',uri);
  if uri = '/' then
  begin
    // Write the output document to the stream
    OutputDataString :=
      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
      + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
      + '<html><h1>Teste</h1></html>' + CRLF;

    ASocket.SendString('HTTP/1.0 200' + CRLF);
    ASocket.SendString('Content-type: Text/Html' + CRLF);
    ASocket.SendString('Content-length: ' + intToStr(length(OutputDataString)) + CRLF);
    ASocket.SendString('Connection: close' + CRLF);
    ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    ASocket.SendString('Server: MNH5 using Synapse' + CRLF);
    ASocket.SendString('' + CRLF);
    ASocket.SendString(OutputDataString);
  end
  else
    ASocket.SendString('HTTP/1.0 404' + CRLF);
end;

VAR
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
begin
  ListenerSocket := TTCPBlockSocket.create;
  ConnectionSocket := TTCPBlockSocket.create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('localhost','1500');
  ListenerSocket.listen;

  repeat
    if ListenerSocket.canread(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      writeln('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
    end;
    writeln('.');
  until false;

  ListenerSocket.free;
  ConnectionSocket.free;
end.
