------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018 Christoph Karl Walter Grein
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
--   http://home.T-Online.de/home/Christ-Usch.Grein/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

package Rational_Arithmetics.Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      5 July 2002
  --====================================================================
  -- Put and Get work like the conventional operations in Text_IO for
  -- numbers.
  -- If the denominator is 1, put writes the number in the format "sn",
  -- else writes it in the format "sn/m" where there is no white space
  -- around '/', and the sign 's' is blank for non-negative numbers.
  -- Get reads the number and raises Data_Error if it does not have the
  -- aforementioned format.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  05.07.2002
  --====================================================================

  procedure Get (Item: out Rational);
  procedure Put (Item: in  Rational);

  procedure Get (File: in File_Type; Item: out Rational);
  procedure Put (File: in File_Type; Item: in  Rational);

end Rational_Arithmetics.Text_IO;
