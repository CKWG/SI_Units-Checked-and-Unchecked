------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2025 Christoph Karl Walter Grein
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

with Ada.Strings.Fixed;

package body Generic_SI.Generic_Temperatures.Generic_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      2 July 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  02.07.2025
  --====================================================================

  function Image (X: Celsius) return String is
    Image: constant String := X.Value'Image;
  begin
    return Image & "°C";
  end Image;

  function Value (X: String) return Celsius is
    Unit_Start: constant Natural := Ada.Strings.Fixed.Index (X, "°");
  begin
    if Unit_Start = 0 then
      raise Unit_Error;
    end if;
    declare
      Num: constant Real'Base := Real'Value (X (X'First .. Unit_Start - 1));  -- will raise Constraint_Error if X is not OK
    begin
      return Num * Ada.Strings.Fixed.Trim (X (Unit_Start .. X'Last), Ada.Strings.Right);  -- will raise Unit_Error if unit is not OK
    end;
  end value;

end Generic_SI.Generic_Temperatures.Generic_Strings;
