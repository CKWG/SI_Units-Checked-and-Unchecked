------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2004, 2006, 2012, 2018 Christoph Karl Walter Grein
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
-- Author's homepage and email address:
--   http://www.christ-usch-grein.homepage.t-online.de/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

with SI.VS;
use  SI.VS, SI;

procedure Ambiguities is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.3
  -- Date      1 October 2006
  --====================================================================
  -- Show ambiguities that would arise if inner and cross products were
  -- operators.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  16.09.2002
  --  C.G.    1.1  30.03.2004 Adapt to new interface
  --  C.G.    1.2  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    1.3  01.10.2018 Output rearranged
  --====================================================================

  V1: constant Vector := (1.0, 2.0, 3.0) * One;
  V2: constant Vector := (3.0, 2.0, 1.0) * One;
  V3: constant Vector := (4.0, 5.0, 6.0) * One;

begin

  declare
    function "*" (Left, Right: Vector) return Item renames Inner;
  begin
    Put_Line ("Operators are evaluated left to right V1 * V2 * V3:");
    Put_Line ("  Result is a vector               inner |");
    Put_Line ("Compare with (V1 * V2) * V3: " &
              Boolean'Image (V1 * V2 * V3 = (V1 * V2) * V3));  -- True
    Put_Line ("Compare with V1 * (V2 * V3): " &
              Boolean'Image (V1 * V2 * V3 = V1 * (V2 * V3)));  -- False
  end;

  New_Line;

  declare
    function "*" (Left, Right: Vector) return Item   renames Inner;
    function "*" (Left, Right: Vector) return Vector renames Cross;
    I: Item;
    -- V: Vector;
  begin
    I := V1 * V2 * V3;
    Put_Line ("Operators are evaluated left to right   I:= V1 * V2 * V3;");
    Put_Line ("  Result is an item                      cross |    | inner");
    Put_Line ("Operators are evaluated left to right: " &
              Boolean'Image (V1 * V2 * V3 = Inner (Cross (V1, V2), V3)));  -- True
    -- V := V1 * V2 * V3;
    -- >>>          ^ ambiguous expression
    -- V := (V1 * V2) * V3;
    -- >>>            ^ ambiguous expression
    -- Interpretation Cross (Cross (V1, V2), V3) or Inner (V1, V2) * V3.
  end;

end Ambiguities;
