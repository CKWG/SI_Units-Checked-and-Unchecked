------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2020, 2025 Christoph Karl Walter Grein
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
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Strings.Fixed,
     Ada.Strings.Maps;

package body Generic_SI.Generic_Unformatted_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.0
  -- Date      15 October 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  29.07.2018
  --  C.G.    2.0  13.05.2020 Parent renamed to Generic_SI
  --  C.G.    3.0  15.08.2025 Value reimplemented
  --  C.G.    3.1  22.08.2025 Ensure_Task_Safety is new
  --  C.G.    4.0  15.10.2025 Renamed from Generic_Strings
  --====================================================================

  function Image (X: Item) return String is
  begin
    return X.Value'Image & Image (X.Unit);
  end Image;

  function Value (X: String) return Item is
    Unit_Start: constant Natural := Ada.Strings.Fixed.Index (X, Ada.Strings.Maps.To_Set ("*/"));
    use Ada.Strings, Ada.Strings.Fixed;
  begin
    if Unit_Start = 0 then
      return Real'Base'Value (X) * One;  -- will raise Constraint_Error if X is not OK
    elsif X (Unit_Start - 1) = ' ' then
      raise Illegal_Unit with "illegal space";
    else
      declare
        Value : constant Real'Base := Real'Base'Value (X (X'First .. Unit_Start - 1));
        Trim_X: constant String    := Trim (X (Unit_Start .. X'Last), Right);
        Result: Item;
        Length: Positive;
        package String_Interface is new Ensure_Task_Safety (Trim_X);  -- make task-safe
      begin
        Construct (String_Interface.Get'Access, Result, Length);
        if Length /= Trim_X'Length then
          raise Illegal_Unit with "string not exhausted";
        end if;
        return Value * Result;
      end;
    end if;
  end Value;

end Generic_SI.Generic_Unformatted_IO;
