//TC#PAS JOB MSGCLASS=A,NOTIFY=&SYSUID
// EXEC PASCG,PARM.COMPILE='L-,D-',PARM.GO=NOPRINT
//SYSIN DD *
(* https://eyeondesign.aiga.org/seasons-greetings-from-paul-rand-milton-      *)
(* glaser-elaine-lustig-cohen-the-eames/                                      *)
(*                                                                            *)
(* Tom Courtos 1958                                                           *)

program SeasonGreetings(output);
var factorial:array[1..65] of 0..9; (* 50! ha 65 cifre *)
    line,number_of_digits:integer;

   procedure DoNextFactorial;
   var index,carry:integer;
   begin
      carry:=0;
      for index:=1 to number_of_digits
      do begin
            carry:=carry+factorial[index]*line;
            factorial[index]:=carry mod 10;
            carry:=carry div 10
         end;
      while carry>0
      do begin
            number_of_digits:=number_of_digits+1;
            factorial[number_of_digits]:=carry mod 10;
            carry:=carry div 10
         end
   end;

   procedure PrintTheFactorial;
   var index:integer;
   begin
      for index:=number_of_digits downto 1
      do write(factorial[index]:1)
   end;

   procedure PrintCenteredLine;
   begin
      (* 63 cifre di 49! + 1 spazio = 64 *)
      write(' ':64-number_of_digits);
      PrintTheFactorial;
      DoNextFactorial;
      write('X',line:2,'=');
      PrintTheFactorial;
      writeln
   end;

   procedure Initialize;
   begin
      factorial[1]:=1; (* 1! = 1 *)
      number_of_digits:=1 (* 1! ha 1 cifra *)
   end;

begin
   Initialize;
   for line:=1 to 50
   do PrintCenteredLine
end.
