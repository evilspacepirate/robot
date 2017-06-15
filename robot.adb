-----------------------------------------------------------------
--                                                             --
-- Robot Utility                                               --
--                                                             --
-- Copyright (c) 2017  John Leimon                             --
--                                                             --
-- Permission to use, copy, modify, and/or distribute          --
-- this software for any purpose with or without fee           --
-- is hereby granted, provided that the above copyright        --
-- notice and this permission notice appear in all copies.     --
--                                                             --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR             --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE       --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY         --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE         --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL         --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS       --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF            --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING      --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      --
-- THIS SOFTWARE.                                              --
--                                                             --
-- Run a command when the time stamp on a set of files is      --
-- changed.                                                    --
--                                                             --
-- Example:                                                    --
--                                                             --
--   robot "gcc hello_world.c" "*.c"                           --
--                                                             --
-----------------------------------------------------------------
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Interfaces.C;
procedure Robot is

   package US renames Ada.Strings.Unbounded;

   RED_TEXT    : constant String := Character'Val (16#1B#) & "[31m";
   GREEN_TEXT  : constant String := Character'Val (16#1B#) & "[32m";
   NORMAL_TEXT : constant String := Character'Val (16#1B#) & "[39m";
   CLEAR_TERM  : constant String := Character'Val (16#1B#) & "c";

   Search_Period : constant Duration := 0.25;

   type Source_Record is record
     Name          : US.Unbounded_String;
     Last_Modified : Time;
   end record;

   type Command_Type is (Clear_Screen, Run_Command, None);

   package Source_Record_Vectors is new
      Indefinite_Vectors (Natural, Source_Record);
   use Source_Record_Vectors;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
      return Command_Type
   is
      Key       : Character;
      Available : Boolean;
   begin
         Get_Immediate (Key, Available);
         if Available then
            case Key is
               when 'r' | 'R' =>
                  return Run_Command;
               when 'c' | 'C' =>
                  return Clear_Screen;
               when others =>
                  return None;
            end case;
         end if;
         return None;
   end Get_Command;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (Command : String)
   is
      use Interfaces.C;
      function System (Arguments : Char_Array) return Integer;
      pragma Import (C, System, "system");
   begin
      if System (To_C(Command)) = 0 then
         Put_Line(GREEN_TEXT & "[SUCCESS]" & NORMAL_TEXT);
      else
         Put_Line(RED_TEXT & "[FAILED]" & NORMAL_TEXT);
      end if;
   end Run_Command;

   ----------
   -- Walk --
   ----------

   procedure Walk
     (Name    : in     String;
      Pattern : in     String;
      Output  : in out Source_Record_Vectors.Vector) 
   is
      procedure Add
        (Item : Directory_Entry_Type)
      is
         Source : Source_Record;
      begin
         Source.Name          := US.To_Unbounded_String (Full_Name (Item));
         Source.Last_Modified := Modification_Time (Full_Name (Item));
         Output.Append(Source);
      end Add;
      procedure Walk
        (Item : Directory_Entry_Type)
      is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), Pattern, Output);
         end if;
      exception
         when Ada.Directories.Name_Error => null;
      end Walk;
   begin
      Search (Name, Pattern, (others => True), Add'Access);
      Search (Name, "", (Directory => True, others => False), Walk'Access);
   end Walk;

   -------------------
   -- Stamp_Sources --
   -------------------

   procedure Stamp_Sources
     (Sources : in out Source_Record_Vectors.Vector)
   is
   begin
      for I in Natural range 2 .. Argument_Count loop
         Walk (".", Argument(I), Sources);
      end loop;
   end Stamp_Sources;

   ----------------
   -- Put_Config --
   ----------------

   procedure Put_Config is
   begin
      Put ("MONITORING: [");
      for I in Natural range 2 .. Argument_Count loop
         Put (Argument (I));
      end loop;
      Put ("] RUN ON MODIFY: [");
      Put (Argument (1));
      Put_Line ("]");
   end Put_Config;

   ---------------------
   -- Execute Command --
   ---------------------

   procedure Execute_Command
      (Command : Command_Type)
   is
   begin
      case Command is
         when Run_Command =>
            Run_Command(Argument(1));
         when Clear_Screen =>
            Put(CLEAR_TERM);
            Put_Config;
         when None =>
            null;
      end case;
   end Execute_Command;

   Old_Fingerprint : Source_Record_Vectors.Vector;
   New_Fingerprint : Source_Record_Vectors.Vector;

   Command         : Command_Type := None;

begin

   if Argument_Count < 2 then
      Put_Line ("Usage: robot <command> <file_pattern> [ .. <file_pattern> ]");
      New_Line;
      Put_Line ("Run time commands:");
      Put_Line (" R-key : Run command");
      Put_Line (" C-key : Clear terminal");
      return;
   end if;

   Put_Config;

   Stamp_Sources (New_Fingerprint);

   loop
      delay Search_Period;
      Old_Fingerprint := New_Fingerprint;
      New_Fingerprint.Clear;
      Stamp_Sources (New_Fingerprint);

      Command := Get_Command;

      if New_Fingerprint /= Old_Fingerprint then
         Command := Run_Command;
      end if;

      Execute_Command (Command);
   end loop;

end Robot;
