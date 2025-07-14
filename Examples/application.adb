------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2006, 2018, 2020, 2025 Christoph Karl Walter Grein
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
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions;

with Rational_Arithmetics;
use  Rational_Arithmetics;

with SI.IO, SI.Nat;
use  SI.IO, SI.Nat, SI;

procedure Application is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.0
  -- Date      12 April 2025
  --====================================================================
  -- Sample program using either the checked or the unchecked variant.
  --
  -- Note that this package remains untouched when switching from
  -- checked to unchecked items.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  23.02.2006
  --  C.G.    2.0  11.04.2006 Vector
  --  C.G.    3.0  05.08.2018 Unit strings
  --  C.G.    3.1  29.02.2020 Improved
  --  C.G.    3.2  15.05.2020 Output improved
  --  C.G.    4.0  12.04.2020 Now Unit_Error raised, was Assertion_Error
  --====================================================================

  function Schottky_Langmuir (Volt: Voltage; Distance: Length) return Current_Density is
  begin
    return (4.0/9.0) * Eps_0 * Sqrt (2.0 * Elementary_Charge / Electron_Mass) *
            Volt**(3/2) / Distance**2;
  end Schottky_Langmuir;

begin

  IO.Default_Fore := 3;
  IO.Default_Aft  := 1;
  IO.Default_Exp  := 0;

  IO.Put (Schottky_Langmuir (5.0 * "V", 2.0 * "mm"), Dim => "uA/mm**2");
  Ada.Text_IO.New_Line;

  begin
    IO.Put (Schottky_Langmuir (5.0 * "mm", 2.0 * "V"));
    Ada.Text_IO.Put_Line (" If you see this message, you are using the unchecked version!");
  exception
    when Ex: Unit_Error => Ada.Text_IO.Put_Line ("As expected: exception Unit_Error (subtype violation) " &
                                                 Ada.Exceptions.Exception_Message (Ex));
  end;

  begin
    IO.Put (Schottky_Langmuir (5.0 * "V", 2.0 * "MM"));
  exception
    when Illegal_Unit => Ada.Text_IO.Put_Line ("As expected: exception Illegal_Unit MM (wrong casing)");
  end;

end Application;
