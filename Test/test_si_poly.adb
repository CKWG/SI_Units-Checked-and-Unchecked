------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2005, 2018, 2020 Christoph Karl Walter Grein
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

with SI.Poly, SI.Strings;
use  SI.Poly, SI.Strings, SI;

with Test_Support;
use  Test_Support;

procedure Test_SI_Poly is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.0
  -- Date      14 May 2020
  --====================================================================
  -- Test polynomials, linear regression and interpolation.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  19.05.1999
  --  C.G.    2.0  26.07.2002 Adapted to SI package
  --  C.G.    2.1  15.11.2005 Renamed from Test_Polynomial_Numerics to
  --                          Test_Unconstrained_Polynomial_Numerics
  --  C.G.    3.0  26.08.2018 Unit strings
  --  C.G.    4.0  14.05.2020 Dimensions generic parameter
  --====================================================================

begin

  Test_Header (Title       => "Test Polynomial_Numerics",
               Description => "Polynomes and linear regression and interpolation.");

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test polynomes",
             Description => "A polynome of degree six is partly factored.");

  declare
    Coeff: constant Coefficients := (3 => 6.0*"m/s**3", 4 => -5.0*"m/s**4", 5 => -2.0*"m/s**5", 6 => 1.0*"m/s**6");
    Exp  : constant Sample       := ((-2.0*"s",   0.0*"m"),
                                     (-1.0*"s",  -8.0*"m"),
                                     ( 0.0*"s",   0.0*"m"),
                                     ( 1.0*"s",   0.0*"m"),
                                     ( 2.0*"s", -32.0*"m"),
                                     ( 3.0*"s",   0.0*"m"));
  begin
    for E of Exp loop
      Assert (Condition => Polynomial (Coeff, E.X) = E.Y,
              Message   => Image (E.X) & " => " & Image (E.Y),
              Only_Report_Error => False);
      Assert (Condition => Polynomial (Coeff, E.X) = (E.X + 2.0*"s") * E.X**3 * (E.X - 1.0*"s") * (E.X - 3.0*"s") * (1.0*"m/s**6"),
              Message   => "   factored",
              Only_Report_Error => False);
    end loop;
  end;

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test linear regression",
             Description => "Take points from a linear function and check that it is reproduced.");

  declare
    SL: constant Straight_Line := (0 => 5.0*"m", 1 => -2.0*"m/s");
    function Line (T: Time) return Length is (Polynomial (SL, T));
    S: Sample (1 .. 10);
  begin
    for I in S'Range loop
      S (I) := (Float (I)*"s", Line (Float (I)*"s"));
    end loop;
    declare
      Coeff: Coefficients renames Linear_Regression (S);
    begin
      Assert (Condition => SL = Coeff,
              Message   => Image (Coeff (0)) & ' ' & Image (Coeff (1)),
              Only_Report_Error => False);
    end;
  end;

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test linear interpolation",
             Description => "");

  declare
    Given : constant Sample := (( 1.0*"s",  0.0*"m"),
                                ( 0.0*"s",  1.0*"m"),
                                (-1.0*"s",  1.0*"m"),
                                (-2.0*"s",  2.0*"m"),
                                (-3.0*"s",  0.0*"m"),
                                (-4.0*"s",  0.0*"m"));
    Expect: constant Sample := (( 2.0*"s", -1.0*"m"),
                                ( 1.0*"s",  0.0*"m"),
                                ( 0.5*"s",  0.5*"m"),
                                ( 0.0*"s",  1.0*"m"),
                                (-0.5*"s",  1.0*"m"),
                                (-1.0*"s",  1.0*"m"),
                                (-1.5*"s",  1.5*"m"),
                                (-2.0*"s",  2.0*"m"),
                                (-2.5*"s",  1.0*"m"),
                                (-3.0*"s",  0.0*"m"),
                                (-3.5*"s",  0.0*"m"),
                                (-4.0*"s",  0.0*"m"),
                                (-4.5*"s",  0.0*"m"));
    Inter: constant Interpolation_Table := Define (Given);
  begin
    for E of Expect loop
      Assert (Condition => Linear_Interpolation (Inter, E.X) = E.Y,
              Message   => Image (E.X) & " => " & Image (E.Y) & "   OK",
              Only_Report_Error => False);
    end loop;
  end;

  -----------------------------------------------------------------------
  Test_Result;

end Test_SI_Poly;
