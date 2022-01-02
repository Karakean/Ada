-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, Customers, and the Kitchen
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 5;
   Number_Of_Dishes: constant Integer := 3;
   Number_Of_Customers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Dish_Type is Integer range 1 .. Number_Of_Dishes;
   subtype Customer_Type is Integer range 1 .. Number_Of_Customers;
   Product_Name: constant array (Product_Type) of String(1 .. 9)
     := ("Macaroni ", "Dough    ", "Olive oil", "Garlic   ", "Tomatoes ");
   Dish_Name: constant array (Dish_Type) of String(1 .. 22)
     := ("Spaghetti aglio e olio", "Penne all'arrabbiata  ", "Pizza Marinara        ");
   package Random_Dish is new
     Ada.Numerics.Discrete_Random(Dish_Type);
   type My_Str is new String(1 ..256);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
   end Producer;

   -- Customer gets an arbitrary Dish of several products from the Kitchen
   task type Customer is
      -- Give the Customer an identity
      entry Start(Customer_Number: in Customer_Type;
		    Consumption_Time: in Integer);
   end Customer;

   -- In the Kitchen, products are assemblied into an Dish
   task type Kitchen is
      -- Accept a product to the kitchen provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer);
      -- Serve an Dish provided there are enough products for it
      entry Serve(Dish: in Dish_Type; Number: out Integer);
   end Kitchen;

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Customers ) of Customer;
   B: Kitchen;

   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production is new
	Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
   begin
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
	 Random_Production.Reset(G);	--  start random number generator
	 Product_Number := 1;
	 Product_Type_Number := Product;
	 Production := Production_Time;
      end Start;
      Put_Line("Started producer of " & Product_Name(Product_Type_Number));
      loop

	 Put_Line("Produced product " & Product_Name(Product_Type_Number)
		    & " number "  & Integer'Image(Product_Number));
	 -- Accept for kitchen
	 B.Take(Product_Type_Number, Product_Number);
	 Product_Number := Product_Number + 1;
      end loop;
   end Producer;

   task body Customer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
	Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;	--  random number generator (time)
      G2: Random_Dish.Generator;	--  also (dishes)
      Customer_Nb: Customer_Type;
      Dish_Number: Integer;
      Consumption: Integer;
      Dish_Type: Integer;
      Customer_Name: constant array (1 .. Number_Of_Customers)
	of String(1 .. 9)
	:= ("Customer1", "Customer2");
   begin
      accept Start(Customer_Number: in Customer_Type;
		     Consumption_Time: in Integer) do
	 Random_Consumption.Reset(G);	--  ustaw generator

	 Customer_Nb := Customer_Number;
	 Consumption := Consumption_Time;
      end Start;
      Put_Line("Started Customer " & Customer_Name(Customer_Nb));
      loop
	 delay Duration(Random_Consumption.Random(G)); --  simulate consumption
	 Dish_Type := Random_Dish.Random(G2);
         -- take an Dish for consumption
    select
            B.Serve(Dish_Type, Dish_Number);
            if Dish_Number /= 0 then
               Put_Line(Customer_Name(Customer_Nb) & ": taken Dish " &
               Dish_Name(Dish_Type) & " number " &
               Integer'Image(Dish_Number));
            else
            Put_Line( "The order of " & Customer_Name(Customer_Nb) &
                      " has been canceled due to lack of ingredients");
            end if;
    else
            Put_Line(Customer_Name(Customer_Nb) &
            " has canceled his order since he was waiting too long");
    end select;
      end loop;
   end Customer;

   task body Kitchen is
      Kitchen_Capacity: constant Integer := 30;
      type Kitchen_type is array (Product_Type) of Integer;
      Kitchen: Kitchen_type
	:= (0, 0, 0, 0, 0);
      Dish_Content: array(Dish_Type, Product_Type) of Integer
	:= ((1, 0, 2, 1, 0),
	    (1, 0, 1, 1, 2),
	    (0, 2, 1, 1, 1));
      Max_Dish_Content: array(Product_Type) of Integer;
      Dish_Number: array(Dish_Type) of Integer
	:= (1, 1, 1);
      In_Kitchen: Integer := 0;
      Rejection: Integer := 0;
      Mess: Integer := 0;

      procedure Setup_Variables is
      begin
	 for W in Product_Type loop
	    Max_Dish_Content(W) := 0;
	    for Z in Dish_Type loop
	       if Dish_Content(Z, W) > Max_Dish_Content(W) then
		  Max_Dish_Content(W) := Dish_Content(Z, W);
	       end if;
	    end loop;
	 end loop;
      end Setup_Variables;



      function Can_Accept(Product: Product_Type) return Boolean is
	 Free: Integer;		--  free room in the kitchen
	 -- how many products are for production of arbitrary Dish
	 Lacking: array(Product_Type) of Integer;
	 -- how much room is needed in kitchen to produce arbitrary Dish
    Lacking_room: Integer;
	 MP: Boolean;			--  can accept
      begin
	 if In_Kitchen >= Kitchen_Capacity then
	    return False;
	 end if;
	 -- There is free room in the kitchen
	 Free := Kitchen_Capacity - In_Kitchen;
	 MP := True;
	 for W in Product_Type loop
	    if Kitchen(W) < Max_Dish_Content(W) then
	       MP := False;
	    end if;
	 end loop;
	 if MP then
	    return True;		--  kitchen has products for arbitrary
	       				--  Dish
	 end if;
	 if Integer'Max(0, Max_Dish_Content(Product) - Kitchen(Product)) > 0 then
	    -- exactly this product lacks
	    return True;
	 end if;
	 Lacking_room := 1;			--  insert current product
	 for W in Product_Type loop
	    Lacking(W) := Integer'Max(0, Max_Dish_Content(W) - Kitchen(W));
	    Lacking_room := Lacking_room + Lacking(W);
	 end loop;
	 if Free >= Lacking_room then
	    -- there is enough room in kitchen for arbitrary Dish
	    return True;
	 else
            -- no room for this product
	    return False;
	 end if;
      end Can_Accept;

      function Can_Serve(Dish: Dish_Type) return Boolean is
      begin
	 for W in Product_Type loop
	    if Kitchen(W) < Dish_Content(Dish, W) then
	       return False;
	    end if;
	 end loop;
	 return True;
      end Can_Serve;

      procedure Kitchen_Contents is
      begin
	 for W in Product_Type loop
	    Put_Line("Kitchen contents: " & Integer'Image(Kitchen(W)) & " "
		       & Product_Name(W));
	 end loop;
      end Kitchen_Contents;



       procedure Kitchen_Clearance is
      begin
         for W in Product_Type loop
            Kitchen(W) := 0;
            In_Kitchen := 0;
         end loop;
         put_line("Someone has stolen all ingredients from Kitchen. " &
                    "Maybe angry producents whose products got spoiled " &
                 "due to rejection?");
         Rejection := 0;
      end Kitchen_Clearance;


      procedure Kitchen_Giveaway is
         Largest_Amount : Integer;
         LAPT : Product_Type; --Largest Amount Product Type
      begin
            Largest_Amount := 0;
            for W in Product_Type loop
               if (Kitchen(W) > Largest_Amount) then
                  Largest_Amount := Kitchen(W);
                  LAPT := W;
               end if;
         end loop;
            Kitchen(LAPT) := Kitchen(LAPT) - 1;
            In_Kitchen := In_Kitchen - 1;
            Put_Line("The restaurant had too much " &
            Product_Name(LAPT) & " so they gave away " &
                       "some of it to people in need.");
    end Kitchen_Giveaway;


   begin
      Put_Line("Kitchen started");
      Setup_Variables;
      loop
         if(Mess >= 10) then
            Put_Line("What a mess! Restaurant staff must " &
                       "clean it up.");
            Mess := 0;
            delay 20.0;
         end if;
	 accept Take(Product: in Product_Type; Number: in Integer) do
	   if Can_Accept(Product) then
	      Put_Line("Accepted product " & Product_Name(Product) & " number " &
                  Integer'Image(Number));
	      Kitchen(Product) := Kitchen(Product) + 1;
	      In_Kitchen := In_Kitchen + 1;
  	   else
	      Kitchen_Giveaway;
      end if;
      Mess := Mess + 1;
	 end Take;
	 Kitchen_Contents;
	 accept Serve(Dish: in Dish_Type; Number: out Integer) do
	    if Can_Serve(Dish) then
	       Put_Line("Served dish " & Dish_Name(Dish) & " number " &
			  Integer'Image(Dish_Number(Dish)));
	       for W in Product_Type loop
		  Kitchen(W) := Kitchen(W) - Dish_Content(Dish, W);
		  In_Kitchen := In_Kitchen - Dish_Content(Dish, W);
	       end loop;
	       Number := Dish_Number(Dish);
	       Dish_Number(Dish) := Dish_Number(Dish) + 1;
	    else
	       Put_Line("Lacking ingredients for cooking " & Dish_Name(Dish));
	       Number := 0;
         end if;
         Mess := Mess + 1;
	 end Serve;
	 Kitchen_Contents;
      end loop;
   end Kitchen;

begin
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Customers loop
      K(J).Start(J,12);
   end loop;
end Simulation;
