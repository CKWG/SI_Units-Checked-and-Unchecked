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

generic

package Generic_SI.Generic_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      11 May 2020
  --====================================================================
  -- For the numeric part, Image and Value behave like the corresponding
  -- attributes.
  -- Image returns the argument as a string, the unit in default
  -- representation; first index 1.
  -- Value returns the argument as an item ignoring any leading or
  -- trailing blanks. Constraint_Error is raised if the numeric string
  -- does not have the correct format, Illegal_Unit is raised if the
  -- unit string does not have the correct format.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  29.07.2018
  --  C.G.    2.0  11.05.2020 Parent renamed to Generic_SI
  --====================================================================

  pragma Elaborate_Body;

  function Image (X: Item  ) return String;
  function Value (X: String) return Item;

end Generic_SI.Generic_Strings;
