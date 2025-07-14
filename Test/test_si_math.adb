------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2020, 2021, 2025 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Source:
-- https://www.adaic.org/ada-resources/tools-libraries/
--   (see Christoph Grein's Essentials)
-- http://archive.adaic.com/tools/CKWG/Dimension/Dimension.html
--
-- Author's email address:
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Assertions;

with Test_Support;
use  Test_Support;

with Rational_Arithmetics;
use  Rational_Arithmetics;

with SI;
use  SI;

procedure Test_SI_Math is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.2
  -- Date      16 April 2025
  --====================================================================
  -- Test correct dimenioning of the mathematical functions.
  -- Note: The test cannot be run with the unchecked version since
  --       dimension output is vital.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.08.2018
  --  C.G.    1.1  28.09.2018 Replaced Unit_Error by Assertion_Error in
  --                          some tests (subtype Dimensionless added)
  --  C.G.    2.0  14.05.2020 Dimensions generic parameter
  --  C.G.    2.1  14.10.2021 Assertion_Error replaced by Unit_Error
  --  C.G.    2.2  16.04.2025 Output improved
  --====================================================================

begin

  Test_Header (Title => "Test SI Math Functions",
               Description => "Test correct function profiles.");

  Test_Step (Title => "Assertions",
             Description => "Test that Assertion_Policy is Check.");

  declare
    T: Time;
  begin
    T := 1.0*"S";
    Assert (Condition => False,
            Message   => "Switch on Assertion_Policy and use checked instantiation",
            Only_Report_Error => False);
    Test_Result;
    return;
  exception
    when Ada.Assertions.Assertion_Error =>  -- Why is this not Unit_Error?
      Assert (Condition => True,
              Message   => "Exception Assertion_Error not as expected",  -- GNAT bug! See below [UA09-009 public]
              Only_Report_Error => False);
  end;

  begin
    declare
      T: Time := 1.0*"S";
    begin
      null;
    end;
    Assert (Condition => False,
            Message   => "Switch on Assertion_Policy and use checked instantiation",
            Only_Report_Error => False);
    Test_Result;
    return;
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Exception Unit_Error as expected",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Test ""**""",
             Description => "Exponent whole, rational, real");

  declare
    R: Float := 4.0;      R2: Float := R**2;
    N: Item  := 4.0*One;  N2: Item  := N**2;
    T: Time  := 4.0*"s";  T2: Item  := T**2;
  begin
    Assert (Condition => R2 = 16.0,
            Message   => "Real Square",
            Only_Report_Error => False);
    Assert (Condition => N2 = 16.0*One,
            Message   => "Number Square",
            Only_Report_Error => False);
    Assert (Condition => T2 = 16.0*"s**2",
            Message   => "Time Square",
            Only_Report_Error => False);
  end;
  New_Line;

  declare
    R: Float := 4.0;      RR: Float := R**(3/2);
    N: Item  := 4.0*"";   NR: Item  := N**(3/2);
    T: Time  := 4.0*"s";  TR: Item  := T**(3/2);
  begin
    Assert (Condition => RR = 4.0,
            Message   => "Real Rat (3/2=1)",
            Only_Report_Error => False);
    Assert (Condition => NR = 8.0*"",
            Message   => "Dimensionless item Rat",
            Only_Report_Error => False);
    Assert (Condition => TR = 8.0*"s**(3/2)",
            Message   => "Time Rat",
            Only_Report_Error => False);
  end;
  New_Line;

  declare
    N: Item  := 4.0*"";  NR: Item := N**2.0;
  begin
    Assert (Condition => NR = 16.0*"",
            Message   => "Real square of dimensionless number",
            Only_Report_Error => False);
  end;
  New_Line;

  declare
    T: Time  := 4.0*"s";
  begin
    Assert (Condition => T**2.0 = 16.0*One and False,
            Message   => "Real square of Time",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Real square of Time Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  declare
    T : Time := 4.0*"s";
    T0: Time := 1.0*"s";
  begin
  --Assert (Condition => (T/T0)**2.0 = 16.0,   should this be included in SI
  --        Message   => "Number Real",
  --        Only_Report_Error => False);
    Assert (Condition => (T/T0)**2.0 = 16.0*"",
            Message   => "Dimensionless fraction real square",
            Only_Report_Error => False);
  end;

  ----------------------------------------------------------

  Test_Step (Title => "Test Exp, Log",
             Description => "Arguments must be dimensionless");

  declare
    Eps: constant Item := 1.0E-6*"";
  begin
    Assert (Condition => Exp (One) = e*"",
            Message   => "Exp 1",
            Only_Report_Error => False);
    Assert (Condition => abs (Log (One*e) - One) <= Eps,
            Message   => "Log e",
            Only_Report_Error => False);
  end;
  New_Line;

  begin
    Assert (Condition => Exp (1.0*"m") = One,  -- result irrelevant
            Message   => "Exp for dimensioned item",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Exp for dimensioned item Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  begin
    Assert (Condition => Log (1.0*"N") = One,  -- result irrelevant
            Message   => "Log for dimensioned item",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Log for dimensioned item Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  begin
    Assert (Condition => Log (10.0*"s", 10.0*One) = One,  -- result irrelevant
            Message   => "Based Log for dimensioned item",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Based Log for dimensioned item Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  begin
    Assert (Condition => Log (10.0*"", 10.0*"s") = One,  -- result irrelevant
            Message   => "Log for dimensioned base",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Log for dimensioned base Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  begin
    Assert (Condition => Log (10.0*"s", 10.0*"s") = One,  -- result irrelevant
            Message   => "Log for dimensioned item and base",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Log for dimensioned item and base Unit_Error expected",
              Only_Report_Error => False);
  end;
  New_Line;

  ----------------------------------------------------------

  Test_Step (Title => "Test Sin and Arcsin",
             Description => "Arguments must be dimensioned correctly.");

  Assert (Condition => Sin ((Pi/2.0)*"rad") = One,
          Message   => "Sin rad",
          Only_Report_Error => False);
  Assert (Condition => Sin ((Pi/2.0)*One) = One,
          Message   => "Sin One",
          Only_Report_Error => False);
  Assert (Condition => Arcsin (One) = (Pi/2.0)*"rad",
          Message   => "Arcsin One",
          Only_Report_Error => False);

  begin
    Assert (Condition => Sin (2.0*"V") = One and False,  -- result irrelevant
            Message   => "Sin for dimensioned item",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Sin for dimensioned item Unit_Error expected",
              Only_Report_Error => False);
  end;

  Assert (Condition => Sin (2.0*"V", 8.0*"V") = One,
          Message   => "Sin correctly dimensioned",
          Only_Report_Error => False);
  Assert (Condition => Arcsin (One, 8.0*"V") = 2.0*"V",
          Message   => "Arcsin correctly dimensioned",
          Only_Report_Error => False);

  begin
    Assert (Condition => Sin (2.0*"V", 8.0*"C") = One and False,
            Message   => "Sin incorrectly dimensioned",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Sin incorrectly dimensioned Unit_Error expected",
              Only_Report_Error => False);
  end;

  Assert (Condition => Arcsin (One, 8.0*"V") /= 2.0*"rad",
          Message   => "Arcsin incorrectly dimensioned",
          Only_Report_Error => False);

  ----------------------------------------------------------

  Test_Step (Title => "Test Hyperbolic Sin",
             Description => "");

  begin
    Assert (Condition => Sinh (8.0*"V") = One,  -- result irrelevant
            Message   => "Sinh dimensioned",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Sinh dimensioned Unit_Error expected",
              Only_Report_Error => False);
  end;

  Test_Result;

end Test_SI_Math;
