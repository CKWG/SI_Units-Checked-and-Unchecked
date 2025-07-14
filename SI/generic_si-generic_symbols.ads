------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2020, 2021 Christoph Karl Walter Grein
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

with Ada.Strings.Maps;

private generic

package Generic_SI.Generic_Symbols is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.2
  -- Date      28 May 2021
  --====================================================================
  -- List of all unit symbols.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  13.05.2020 Separated from Generic_Strings
  --  C.G.    1.1  20.05.2020 Work-around for GNAT CE 2020 bug
  --  C.G.    1.2  28.05.2021 Work-around for [T520-013 public] removed
  --====================================================================

  pragma Elaborate_Body;

  -- Set of legal characters in symbols:
  function Legal_Characters return Ada.Strings.Maps.Character_Set with Inline;

  function Evaluate (Symbol: String) return Item with Pre => Symbol'First = 1;

  function Image (X: Dimension) return String;

end Generic_SI.Generic_Symbols;
