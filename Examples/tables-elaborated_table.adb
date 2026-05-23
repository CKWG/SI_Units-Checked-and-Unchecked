------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
-- Copyright (C) 2026 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

separate (Tables)
package body Elaborated_Table is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      13 May 2026
  --====================================================================
  -- Column 1 .. Start-1 => Name
  -- Column Start        => Width characers for X in given numeric
  --                        format, Pad blanks, Unit characters for the
  --                        unit left or right aligned
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  13.05.2026
  --====================================================================

  S   : Positive_Count;
  W, P: Field;
  U   : Field'Base;

  procedure Set (Start: in  Positive_Count; Width, Pad: in Field; Unit: in  Field'Base) is
  begin
    S := Start;
    W := Width;
    P := Pad;
    U := Unit;
  end Set;

  procedure Put (Name : in  String; X: in  Item; Aft, Exp: in Field; Dim: in String := "") is
    E: constant Field := (if Exp > 0 then Field'Max (3, Exp) else 0);
    F: constant Field := W - Aft - E - 1 - (if Exp = 0 then 0 else 1);  -- -1 for sign and exponent character E
  begin
    Put (Name);
    Set_Col (S);
    Put (X, F, Aft, E, Dim, P, U);
    New_Line;
  end Put;

  procedure Get (Name : out String; X: out Item) is
  begin
    Get (Name (Name'First .. Name'First + Positive (S) - 2));
    Get (X, W, P, U);
    Skip_Line;
  end Get;

end Elaborated_Table;
