------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2025 Christoph Karl Walter Grein
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

with Ada.Text_IO;
use  Ada.Text_IO;

generic

  with package Real_Text_IO is new Float_IO (Real'Base);

package Generic_SI.Generic_Temperatures.Generic_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      3 July 2025
  --====================================================================
  -- Get and Put read and write the numeric value like standard Text_IO.
  --
  -- Get: If no exception is raised, reading continues until the next
  -- space character or line end (resp. string end), whichever comes
  -- first, is reached, which stays in the input stream. If any other
  -- string than "°C" has been read, Illegal_Unit will be raised. Last
  -- is the index of the last character read (for string).
  --
  -- Put: If no exception is raised, writes the unit °C with no leading
  -- (nor trailing) space - this is different to the SI units.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  08.10.2002
  --  C.G.    2.0  01.07.2025 Resurrect unconstrained_checked_si-
  --                          generic_temperatures-generic_text_io and
  --                          rename; overhaul
  --  C.G.    2.1  03.07.2025 Get, Put for Strings
  --====================================================================

  pragma Elaborate_Body;

  Default_Fore: Field := 2;
  Default_Aft : Field := Real'Digits - 1;
  Default_Exp : Field := 3;

  procedure Get (X    : out Celsius;
                 Width: in  Field := 0);
  procedure Get (File : in  File_Type;
                 X    : out Celsius;
                 Width: in  Field := 0);

  procedure Put (X   : in Celsius;
                 Fore: in Field := Default_Fore;
                 Aft : in Field := Default_Aft;
                 Exp : in Field := Default_Exp);
  procedure Put (File: in File_Type;
                 X   : in Celsius;
                 Fore: in Field := Default_Fore;
                 Aft : in Field := Default_Aft;
                 Exp : in Field := Default_Exp);

  procedure Get (From: in  String;
                 X   : out Celsius;
                 Last: out Positive);
  procedure Put (To  : out String;
                 X   : in  Celsius;
                 Aft : in  Field  := Default_Aft;
                 Exp : in  Field  := Default_Exp);

end Generic_SI.Generic_Temperatures.Generic_Text_IO;
