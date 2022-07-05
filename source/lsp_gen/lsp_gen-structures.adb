------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;

with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Structures is

   use all type LSP_Gen.Entities.Enum.AType_Variant;

   package String_Sets is new Ada.Containers.Ordered_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings."<",
      VSS.Strings."=");

   procedure Find_Optional_And_Arrays;
   procedure Write_Mixins;
   procedure Write_Mixin (Item : LSP_Gen.Entities.Structure);
   procedure Write_Type
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   ------------------------------
   -- Find_Optional_And_Arrays --
   ------------------------------

   procedure Find_Optional_And_Arrays is
   begin
      for Item of Types loop
         for J in 1 .. Item.Definition.properties.Length loop
            declare
               Property : constant LSP_Gen.Entities.Property :=
                 Item.Definition.properties (J);
            begin
               if Property.optional then
                  if Property.a_type.Union.Kind = reference then
                     Types (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  end if;
               elsif Property.a_type.Union.Kind = an_array then
                  declare
                     Element : constant LSP_Gen.Entities.AType :=
                       Property.a_type.Union.an_array.element.Value;
                  begin
                     case Element.Union.Kind is
                        when base =>
                           null;
                        when reference =>
                           Types (Element.Union.reference.name).Has_Array :=
                             True;
                        when others =>
                           raise Program_Error;
                     end case;
                  end;
               end if;
            end;
         end loop;
      end loop;
   end Find_Optional_And_Arrays;

   -----------------
   -- Write_Mixin --
   -----------------

   procedure Write_Mixin (Item : LSP_Gen.Entities.Structure) is
   begin
      Put ("type ");
      Put_Id (Item.name);
      Put_Line (" is interface;");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;

      for J in 1 .. Item.properties.Length loop
         declare
            Property : constant LSP_Gen.Entities.Property :=
              Item.properties (J);
         begin
            Put ("--  function ");
            Put_Id (Property.name);
            Put (" (Self : ");
            Put_Id (Item.name);
            Put (") return ");
            Put_Type (Property.a_type);
            Put_Line (" is abstract;");
            Put_Lines (Property.documentation.Split_Lines, "   --  ");
            New_Line;
         end;
      end loop;
   end Write_Mixin;

   ------------------
   -- Write_Mixins --
   ------------------

   procedure Write_Mixins is
      Set : String_Sets.Set;
   begin
      for Item of Types loop
         for K in 1 .. Item.Definition.mixins.Length loop
            declare
               Mixin : constant LSP_Gen.Entities.AType :=
                 Item.Definition.mixins (K);
            begin
               pragma Assert (Mixin.Union.Kind = reference);

               Set.Include (Mixin.Union.reference.name);
            end;
         end loop;
      end loop;

      for Name of Set loop
         Types (Name).Is_Mixin := True;
         Write_Mixin (Types (Name).Definition);
      end loop;
   end Write_Mixins;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set)
   is
      Item : constant LSP_Gen.Entities.Structure := Types (Name).Definition;
   begin
      if Done.Contains (Name) or else Types (Name).Is_Mixin then
         return;
      end if;

--      for J in 1 .. Item.extends

      Done.Insert (Name);
      Put ("type ");
      Put_Id (Name);
      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Type;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types (List : LSP_Gen.Entities.Structure_Vector) is
      Done : String_Sets.Set;
   begin
      Put_Line ("package LSP.Structures is"); New_Line;

      for J in 1 .. List.Length loop
         declare
            Item : constant LSP_Gen.Entities.Structure := List (J);
         begin
            Types.Insert (Item.name, (Definition => Item, others => <>));
         end;
      end loop;

      Write_Mixins;
      Find_Optional_And_Arrays;

      for Cursor in Types.Iterate loop
         Write_Type (Type_Maps.Key (Cursor), Done);
         --  declare
         --     Item : constant LSP_Gen.Entities.Structure :=
         --       Type_Maps.Element (Cursor).Definition;
         --  begin
         --     if Item.extends.Length > 1 and then
         --       Types (Item.extends (2).Union.reference.name).Definition
         --         .properties.Length > 1
         --     then
         --        Put ("-- XXX: ");
         --        Put_Line (Item.name);
         --     end if;
         --  end;
      end loop;

      Put_Line ("end LSP.Structures;");
   end Write_Types;

end LSP_Gen.Structures;
