------------------------------------------------------------------------
-- This software is provided as is in the hope that it might be found
-- useful.
-- You may use and modify it freely provided you keep this copyright
-- notice unchanged and mark modifications appropriately.
--
-- Bug reports and proposals for improvements are welcome. Please send
-- them to the eMail address below.
--
-- Christoph Karl Walter Grein
-- Hauptstr. 42
-- D-86926 Greifenberg
-- Germany
--
-- eMail:    Christ-Usch.Grein@T-Online.de
-- Internet: http://home.T-Online.de/home/Christ-Usch.Grein
--
-- Copyright (c) 1994, 1997, 1998, 2000, 2006, 2008
-- Christoph Karl Walter Grein
------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Text_IO;
use  Ada.Text_IO;

package body Test_Support is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      15 May 2008
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author Version   Date    Reason for Change
  --  C.G.   1.0  15.03.1994  Original version included in some test
  --                          program
  --  C.G.   1.1  21.02.1997  Extracted from the test program for
  --                          general use
  --  C.G.   1.2  25.02.1997  Refined
  --  C.G.   1.3  15.03.1997  Bug fix and improvements
  --  C.G.   1.4  22.03.1997  Add Empty_Line and Put_Line
  --  C.G.   1.5  19.10.1998  Formatting output (break long lines)
  --  C.G.   1.6  30.09.2000
  --  C.G.   2.0  20.10.2006  Ada 2005: Add date of test
  --  C.G.   2.1  01.12.2006  Reset with Test_Result
  --  C.G.   3.0  15.05.2008  Add Global_Result
  --====================================================================

  Steps, Assertions, Failed, Global: Natural := 0;
  Fail: Boolean := False;

  procedure New_Line (Spacing: in Positive := 1) is
  begin
    New_Line (Positive_Count (Spacing));
  end New_Line;

  procedure Empty_Line (Spacing: in Positive_Count := 1) renames New_Line;
  -- to resolve ambiguity for call of New_Line.

  procedure Put_Line (Text: in String) renames Ada.Text_IO.Put_Line;

  procedure Put_Separator is
  begin
    Put_Line ((1..60 => '-'));
  end Put_Separator;

  procedure Break_Long_Lines (Text: in String) is
    -- If lines do not fit on 60 columns, break them at the last blank
    -- before this column.
    Offset  : constant := 14;  -- to align properly under "Description: "
    First   : Natural := Text'First;
    Last    ,
    Break_At: Natural;  -- Text might be empty (Text'Last = Last = 0)
  begin
    loop
      Last := Integer'Min (Text'Last, First + 60 - Offset);
      if Text'Last > Last then
        Break_At := Ada.Strings.Fixed.Index (Source  => Text (First .. Last),
                                             Pattern => " ",
                                             Going   => Ada.Strings.Backward);
      else
        Break_At := Text'Last;
      end if;
      Put_Line (Text (First .. Break_At));
      exit when Break_At = Text'Last;
      First := Break_At + 1;
      Set_Col (Offset);
    end loop;
  end Break_Long_Lines;

  procedure Test_Header (Title, Description: in String) is
  begin
    Put_Separator;
    Put_Line ("Header");
    Put_Line ("======");
    Put_Line ("Title      : " & Title);
    Put      ("Description: ");  Break_Long_Lines (Description);
    Put      ("Date       : ");  Put_Line (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock, Include_Time_Fraction => True));
    Empty_Line;
  end Test_Header;

  procedure Test_Step (Title, Description: in String) is
  begin
    Steps := Steps + 1;
    Empty_Line;
    Put_Separator;
    Put_Line ("Step" & Integer'Image (Steps));
    Put_Line ("Title      : " & Title);
    Put      ("Description: ");  Break_Long_Lines (Description);
    Empty_Line;
  end Test_Step;

  procedure Assert (Condition: in Boolean; Message: in String;
                    Only_Report_Error: in Boolean := True) is
  begin
    Assertions := Assertions + 1;
    if not Condition then
      Put (Message);  Set_Col (70);  Put_Line ("Failed");
      Failed := Failed + 1;
      Fail   := True;
    elsif not Only_Report_Error then
      Put (Message);  Set_Col (70);  Put_Line ("OK");
    end if;
  end Assert;

  procedure Test_Result is
  begin
    Empty_Line;
    Put_Separator;
    Put_Line ("Test result");
    Put_Line ("===========");
    Put_Line ("Steps     :" & Integer'Image (Steps));
    Put_Line ("Assertions:" & Integer'Image (Assertions));
    Put_Line ("Failed    :" & Integer'Image (Failed));
    Put_Line ("===========");
    if Fail then
      Put_Line ("Test failed");
      Global := Global + 1;
    else
      Put_Line ("Test passed");
    end if;
    Put_Line ("===========");
    -- Reset for next test:
    Steps      := 0;
    Assertions := 0;
    Failed     := 0;
    Fail       := False;
  end Test_Result;

  procedure Global_Result is
  begin
    Empty_Line;
    Put_Separator;
    Put_Line ("Global result");
    Put_Line ("=============");
    if Global = 0 then
      Put_Line ("All tests passed.");
    else
      Put_Line ("Failed tests:" & Integer'Image (Global));
    end if;
    Put_Line ("=============");
  end Global_Result;

end Test_Support;
