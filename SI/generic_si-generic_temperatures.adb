------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2025 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
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
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Characters.Conversions;

with Generic_SI.Generic_Temperatures.Generic_Unformatted_IO;

package body Generic_SI.Generic_Temperatures is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      20 October 2025
  --====================================================================
  -- I do not really understand what there is going on.
  -- Only Latin_1 characters are used, so why does the call
  --   B.Put (Unformatted_IO.Image (X));
  -- raise ADA.STRINGS.UTF_ENCODING.ENCODING_ERROR?
  -- Culprit is the character '°'.
  -- Conversion to Wide_Wide_String works correctly, but attribute
  -- 'Image returns String, not Wide_Wide_String.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  20.10.2025 Redefine 'Image attribute
  --====================================================================

  package Unformatted_IO is new Generic_Temperatures.Generic_Unformatted_IO;

  function Value (X: String) return Celsius renames Unformatted_IO.Value;

  procedure Image (B: in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; X: Celsius) is
  begin
    B.Wide_Wide_Put (Ada.Characters.Conversions.To_Wide_Wide_String (Unformatted_IO.Image (X)));
  end Image;

end Generic_SI.Generic_Temperatures;
