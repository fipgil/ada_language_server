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
   use type LSP_Gen.Entities.Enum.BaseTypes;
   use type VSS.Strings.Virtual_String;

   package String_Sets is new Ada.Containers.Ordered_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings."<",
      VSS.Strings."=");

   procedure Find_Optional_And_Arrays;
   procedure Write_Mixins;
   procedure Write_Mixin (Item : LSP_Gen.Entities.Structure);
   procedure Write_Structure
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Type_Alias
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Properties (List : LSP_Gen.Entities.Property_Vector);
   procedure Write_Property (Item : LSP_Gen.Entities.Property);
   procedure Write_Type_Name
     (Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean);
   procedure Write_Or_Type
     (List : LSP_Gen.Entities.AType_Vector;
      Done : in out String_Sets.Set);
   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.Property_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   Base_Short_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "IRI",
        LSP_Gen.Entities.Enum.integer     => "Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Boolean",
        LSP_Gen.Entities.Enum.a_null      => "???");

   Base_Full_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "VSS.IRIs.IRI",
        LSP_Gen.Entities.Enum.integer     => "Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "VSS.Strings.Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Boolean",
        LSP_Gen.Entities.Enum.a_null      => "???");

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      case Item.Union.Kind is
         when base | stringLiteral =>
            null;
         when reference =>
            if Skip /= Item.Union.reference.name then
               if Types.Contains (Item.Union.reference.name) then
                  Write_Structure (Item.Union.reference.name, Done);
               elsif Aliases.Contains (Item.Union.reference.name) then
                  Write_Type_Alias (Item.Union.reference.name, Done);
               end if;
            end if;
         when an_array =>
            Emit_Dependence (Item.Union.an_array.element.Value, Skip, Done);

         when a_or =>
            Emit_Dependence (Item.Union.a_or.items, Skip, Done);
            Write_Or_Type (Item.Union.a_or.items, Done);

         when literal =>
            Emit_Dependence (Item.Union.literal.value.properties, Skip, Done);

         when map =>
            Emit_Dependence (Item.Union.map.value.Value, Skip, Done);

         when tuple =>
            Emit_Dependence (Item.Union.tuple.items, Skip, Done);

         when an_and |
              integerLiteral | booleanLiteral =>
            raise Program_Error;
      end case;
   end Emit_Dependence;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence (Item (J), Skip, Done);
      end loop;
   end Emit_Dependence;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.Property_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence (Item (J).a_type, Skip, Done);
      end loop;
   end Emit_Dependence;

   ------------------------------
   -- Find_Optional_And_Arrays --
   ------------------------------

   procedure Find_Optional_And_Arrays is

      procedure Process (Item : LSP_Gen.Entities.AType);
      procedure Process (Item : LSP_Gen.Entities.AType_Vector);
      procedure Process (Item : LSP_Gen.Entities.Property_Vector);

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.AType) is
      begin
         case Item.Union.Kind is
            when base | reference | stringLiteral =>
               null;
            when an_array =>
               declare
                  Element : constant LSP_Gen.Entities.AType :=
                    Item.Union.an_array.element.Value;
               begin
                  case Element.Union.Kind is
                     when reference =>
                        if Types.Contains (Element.Union.reference.name) then
                           Types (Element.Union.reference.name).Has_Array :=
                             True;
                        elsif Aliases.Contains
                          (Element.Union.reference.name)
                        then
                           Aliases (Element.Union.reference.name).Has_Array :=
                             True;
                        end if;
                     when others =>
                        Process (Element);
                  end case;
               end;

            when a_or =>
               Process (Item.Union.a_or.items);

            when literal =>
               Process (Item.Union.literal.value.properties);

            when map =>
               Process (Item.Union.map.value.Value);

            when tuple =>
               Process (Item.Union.tuple.items);

            when an_and |
                 integerLiteral | booleanLiteral =>
               raise Program_Error;
         end case;
      end Process;

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.AType_Vector) is
      begin
         for J in 1 .. Item.Length loop
            Process (Item (J));
         end loop;
      end Process;

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.Property_Vector) is
      begin
         for J in 1 .. Item.Length loop
            declare
               Property : constant LSP_Gen.Entities.Property :=
                 Item (J);
            begin
               if Property.optional and then
                 Property.a_type.Union.Kind = reference
               then
                  if Types.Contains
                    (Property.a_type.Union.reference.name)
                  then
                     Types
                       (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  elsif Aliases.Contains
                    (Property.a_type.Union.reference.name)
                  then
                     Aliases
                       (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  end if;
               end if;

               Process (Property.a_type);
            end;
         end loop;

      end Process;

   begin
      for Item of Types loop
         Process (Item.Definition.extends);
         Process (Item.Definition.mixins);
         Process (Item.Definition.properties);

         for K in 1 .. Item.Definition.mixins.Length loop
            declare
               Mixin : constant LSP_Gen.Entities.AType :=
                 Item.Definition.mixins (K);
            begin
               pragma Assert (Mixin.Union.Kind = reference);

               Types (Mixin.Union.reference.name).Is_Mixin := True;
            end;
         end loop;

         for K in 1 .. Item.Definition.extends.Length loop
            declare
               Base : constant LSP_Gen.Entities.AType :=
                 Item.Definition.extends (K);
            begin
               pragma Assert (Base.Union.Kind = reference);

               Types (Base.Union.reference.name).Is_Tagged := True;
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
   begin
      for Item of Types loop
         if Item.Is_Mixin then
            Write_Mixin (Item.Definition);
         end if;
      end loop;
   end Write_Mixins;

   -------------------
   -- Write_Or_Type --
   -------------------

   procedure Write_Or_Type
     (List : LSP_Gen.Entities.AType_Vector;
      Done : in out String_Sets.Set)
   is
      Last     : constant LSP_Gen.Entities.AType := List (List.Length);
      Has_Null : constant Boolean := Last.Union.Kind = base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      if List.Length = 2 and then
        Has_Null and then
        List (1).Union.Kind = LSP_Gen.Entities.Enum.base
      then
         declare
            Item_Name : constant VSS.Strings.Virtual_String :=
              Base_Short_Name (List (1).Union.base.name);
            Type_Name : constant VSS.Strings.Virtual_String :=
              Item_Name & "_Or_Null";
         begin
            if not Done.Contains (Type_Name) then
               Done.Insert (Type_Name);

               Put ("type ");
               Put (Type_Name);
               Put_Line (" (Is_Null : Boolean := True) is record");
               Put_Line ("case Is_Null is");
               Put_Line ("when True =>");
               Put_Line ("null;");
               Put_Line ("when False =>");
               Put ("Value : ");
               Put (Base_Full_Name (List (1).Union.base.name));
               Put_Line (";");
               Put_Line ("end case;");
               Put_Line ("end record;");
               New_Line;
            end if;
         end;
      end if;
   end Write_Or_Type;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties (List : LSP_Gen.Entities.Property_Vector) is
   begin
      for J in 1 .. List.Length loop
         Write_Property (List (J));
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property (Item : LSP_Gen.Entities.Property) is
   begin
      Put_Id (Item.name);
      Put (" : ");
      Write_Type_Name (Item.a_type, Item.optional);
      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Property;

   ---------------------
   -- Write_Structure --
   ---------------------

   procedure Write_Structure
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set)
   is
      Item : constant LSP_Gen.Entities.Structure := Types (Name).Definition;
   begin
      if Done.Contains (Name) or else Types (Name).Is_Mixin then
         return;
      end if;

      Emit_Dependence (Item.extends, Item.name, Done);
      Emit_Dependence (Item.mixins, Item.name, Done);
      Emit_Dependence (Item.properties, Item.name, Done);

      Done.Insert (Name);
      Put ("type ");
      Put_Id (Name);
      Put_Line (" is ");

      if Item.extends.Length > 0 then
         pragma Assert (Item.extends.Length = 1);
         Put ("new ");
         Put_Id (Item.extends (1).Union.reference.name);
         Put (" with ");
      elsif Types (Name).Is_Tagged then
         Put ("tagged ");
      end if;

      Put_Line ("record");
      Write_Properties (Item.properties);
      Put_Line ("end record;");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Structure;

   ---------------------
   -- Write_Type_Name --
   ---------------------

   procedure Write_Type_Name
     (Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean)
   is
   begin
      pragma Assert (not Is_Optional);
      case Item.Union.Kind is
         when base =>
            Put (Base_Full_Name (Item.Union.base.name));
         when reference =>
            Put ("LSP.Structures.");
            Put_Id (Item.Union.reference.name);
         when a_or =>
            declare
               List : constant LSP_Gen.Entities.AType_Vector :=
                 Item.Union.a_or.items;

               Last : constant LSP_Gen.Entities.AType := List (List.Length);

               Has_Null : constant Boolean := Last.Union.Kind = base
                 and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
            begin
               if List.Length = 2 and then
                 Has_Null and then
                 List (1).Union.Kind = LSP_Gen.Entities.Enum.base
               then
                  Put (Base_Full_Name (List (1).Union.base.name));
               else
                  raise Program_Error;
               end if;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Write_Type_Name;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types (Model : LSP_Gen.Entities.MetaModel) is
      List    : LSP_Gen.Entities.Structure_Vector renames Model.structures;
      Aliases : LSP_Gen.Entities.TypeAlias_Vector renames Model.typeAliases;

      Done : String_Sets.Set;
   begin
      --  Put structures into Types
      for J in 1 .. List.Length loop
         declare
            Item : constant LSP_Gen.Entities.Structure := List (J);
         begin
            Types.Insert (Item.name, (Item.name, Item, others => <>));
         end;
      end loop;

      for J in 1 .. Aliases.Length loop
         declare
            Item : constant LSP_Gen.Entities.TypeAlias := Aliases (J);
         begin
            LSP_Gen.Type_Aliases.Aliases.Insert
              (Item.name, (Item.name, Item, others => <>));
         end;
      end loop;

      Find_Optional_And_Arrays;
      Put_Line ("with VSS.Strings;"); New_Line;
      Put_Line ("with VSS.IRIs;"); New_Line;

      Put_Line ("package LSP.Structures is"); New_Line;

      Write_Mixins;

      for Cursor in Types.Iterate loop
         Write_Structure (Type_Maps.Key (Cursor), Done);
      end loop;

      Put_Line ("end LSP.Structures;");
   end Write_Types;

   ----------------------
   -- Write_Type_Alias --
   ----------------------

   procedure Write_Type_Alias
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set)
   is
      Item : constant LSP_Gen.Entities.TypeAlias := Aliases (Name).Definition;
   begin
      if Done.Contains (Name) then
         return;
      end if;

      if Name /= "LSPAny" and Name /= "LSPArray" then
         Emit_Dependence (Item.a_type, "", Done);
      end if;

      Done.Insert (Name);
      Put ("type ");
      Put_Id (Name);

      case Item.a_type.Union.Kind is
         when base =>
            Put (" is new ");
            case Item.a_type.Union.base.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put ("VSS.Strings.Virtual_String with null record");
               when others =>
                  raise Program_Error;
            end case;
         when others =>
            raise Program_Error;
      end case;

      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Type_Alias;

end LSP_Gen.Structures;
