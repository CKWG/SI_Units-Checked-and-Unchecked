------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2020 Christoph Karl Walter Grein
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

with Ada.Strings.Fixed,
     Ada.Strings.Maps;

package body Generic_SI.Generic_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      13 May 2020
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  29.07.2018
  --  C.G.    2.0  13.05.2020 Parent renamed to Generic_SI
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
      raise Illegal_Unit;
    elsif X (Unit_Start) = '*' then
      return Real'Base'Value (X (X'First .. Unit_Start - 1)) * Trim (X (Unit_Start + 1 .. X'Last), Right);
    else
      return Real'Base'Value (X (X'First .. Unit_Start - 1)) / Trim (X (Unit_Start + 1 .. X'Last), Right);
    end if;
  end Value;

end Generic_SI.Generic_Strings;
