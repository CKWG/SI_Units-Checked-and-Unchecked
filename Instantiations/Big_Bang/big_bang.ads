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

with Generic_Universe;

package Big_Bang is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      3 May 2026
  --====================================================================
  -- Alternative instantiation.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  03.05.2026
  --====================================================================

  -- From Gnat package Standard
  type Float_18 is digits 18 range -16#0.FFFF_FFFF_FFFF_FFFF#E4096 .. 16#0.FFFF_FFFF_FFFF_FFFF#E4096;
  for  Float_18'Size use 128;

  package Universe is new Generic_Universe (Float_18);

end Big_Bang;
