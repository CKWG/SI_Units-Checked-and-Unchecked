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
-- Copyright (c) 1994, 1997, 2008 Christoph Karl Walter Grein
------------------------------------------------------------------------

package Test_Support is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      15 May 2008
  --====================================================================
  -- A test can be divided into several steps. It looks like so:
  --
  -- procedure Test is
  -- begin
  --   Test_Header;
  --   repeat the folowing steps as often an appropriate
  --     Test_Step;
  --     call UUT;  -- Unit Under Test
  --     Assert;
  --   Test_Result;
  --
  -- Several tests may be combined in one supertest. This looks like so:
  --
  --   Test_1;
  --   ...
  --   Test_n;
  --   Global_Result;
  --====================================================================
  -- History:
  -- Author Version   Date    Reason for Change
  --  C.G.   1.0  15.03.1994  Original version included in some test
  --                          program
  --  C.G.   1.1  21.02.1997  Extracted from the test program for
  --                          general use
  --  C.G.   1.2  25.02.1997  Refined
  --  C.G.   1.3  22.03.1997  Add New_Line and Put_Line
  --  C.G.   2.0  15.05.2008  Add Global_Result
  --====================================================================

  procedure Test_Header (Title, Description: in String);
  procedure Test_Step   (Title, Description: in String);

  procedure New_Line (Spacing: in Positive := 1);
  procedure Put_Line (Text   : in String);

  procedure Assert (Condition        : in Boolean;
                    Message          : in String;
                    Only_Report_Error: in Boolean := True);

  procedure Test_Result;
  procedure Global_Result;

end Test_Support;
