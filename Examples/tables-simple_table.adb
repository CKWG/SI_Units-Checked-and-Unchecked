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
package body Simple_Table is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      12 May 2026
  --====================================================================
  -- Column 1 .. Start-1 => Name
  -- Column Start        => X in given format
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  04.05.2026 Made separate
  --  C.G.    1.1  12.05.2026 Bug fix
  --====================================================================

  S: Positive_Count;

  procedure Set (Start: in Positive_Count) is
  begin
    S := Start;
  end Set;

  procedure Put (Name: in String; X: in Item; Fore, Aft, Exp: in Field; Dim: in String := "") is
  begin
    Put (Name);
    Set_Col (S);
    Put (X, Fore, Aft, Exp, Dim => Dim);
    New_Line;
  end Put;

  procedure Get (Name: out String; X: out Item) is
  begin
    Get (Name (Name'First .. Name'First + Positive (S) - 2));
    Get (X);
    Skip_Line;
  end Get;

end Simple_Table;
