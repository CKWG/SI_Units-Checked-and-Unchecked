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

with Ada.Text_IO;
use  Ada.Text_IO;

with SI.Nat, SI.IO;
use  SI.Nat, SI.IO, SI;

procedure Higgs is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      28 February 2026
  --====================================================================
  -- From:
  -- Spektrum der Wissenschaft 3.26 (the German edition of Scientific
  -- American)
  -- Manon Bischoff - When the Simpsons predicted the mass of the Higgs
  -- boson.
  --
  -- This is just for fun.
  -- Float (digits 6) is not exact enough to produce the correct values,
  -- Long_Float (digits 15) is near; Long_Long_Float (digits 18) is
  -- appropriate.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  28.02.2026 Just for fun
  --====================================================================

  Higgs_Mass: constant Energy := 125.0*"GeV";                         -- measured 2012
  Simpsons  : constant Mass   := Pi * Alpha**8 * Sqrt (h*c/Gravity);  -- predicted 20 Sept 1988, about 775 GeV
  Improved  : constant Mass   := Simpsons / 6.0;

begin

  Put (Higgs_Mass     , Aft => 1, Exp => 0, Dim => "GeV");  Put_Line (" measured");   --   Float     Long_Float
  Put (Simpsons * c**2, Aft => 1, Exp => 0, Dim => "GeV");  Put_Line (" predicted");  -- 1113.7*GeV   773.1*GeV
  Put (Improved * c**2, Aft => 1, Exp => 0, Dim => "GeV");  Put_Line (" improved");   --  185.6*GeV   128.9*GeV

end Higgs;
