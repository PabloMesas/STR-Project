-- Error in samples has been corrcted

with Kernel.Serial_Output; use Kernel.Serial_Output;
with System; use System;
with tools; use tools;

package body devices is

    WCET_Car_Distance: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(10);
    WCET_Eyes_Image: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(40);
    WCET_EEG: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(30);
    WCET_Display: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(15);
    WCET_Alarm: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(5);
    WCET_Light: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(5);
    WCET_Automatic_Driving: constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(10);

    EYES_REACTION_WHEN_BEEP: constant integer := 1;
    -- 0 = no reaction
    -- 1 = short reaction
    -- 2 = large reaction 

   --------------------------------------------------------------------------
   -- Procedures to access electrode sensors
   --------------------------------------------------------------------------

    cantidad_datos_Sensores: constant := 50;
    type Indice_Secuencia_Sensores is mod cantidad_datos_Sensores;
    type tipo_Secuencia_Sensores is array (Indice_Secuencia_Sensores) of EEG_Samples_Type;

    protected Sensores_Electrodos is
       procedure Reading_Sensors (L: out EEG_Samples_Type);
    private
      i: Indice_Secuencia_Sensores := 1;
      Secuencia: tipo_Secuencia_Sensores := (
       (7,7,7,7,7,7,7,7,7,7),(7,7,7,7,7,7,7,7,7,7),
       (7,7,7,7,7,7,7,7,7,7),(7,7,7,7,7,7,7,7,7,7),
       (7,7,7,7,7,7,7,7,7,7),(8,8,8,8,8,8,8,8,8,8),
       (8,8,8,8,8,8,8,8,8,8),(8,8,8,8,8,8,8,8,8,8),
       (8,8,8,8,8,8,8,8,8,8),(8,8,8,8,8,8,8,8,8,8),

       (4,4,4,4,4,4,4,4,4,4),(4,4,4,4,4,4,4,4,4,4),
       (4,4,4,4,4,4,4,4,4,4),(5,5,5,5,5,5,5,5,5,5),
       (5,5,5,5,5,5,5,5,5,5),(6,6,6,6,6,6,6,6,6,6),
       (6,6,6,6,6,6,6,6,6,6),(6,6,6,6,6,6,6,6,6,6),
       (6,6,6,6,6,6,6,6,6,6),(6,6,6,6,6,6,6,6,6,6),

       (1,1,1,1,1,1,1,1,1,1),(1,1,1,1,1,1,1,1,1,1),
       (1,1,1,1,1,1,1,1,1,1),(2,2,2,2,2,2,2,2,2,2),
       (2,2,2,2,2,2,2,2,2,2),(2,2,2,2,2,2,2,2,2,2),
       (2,2,2,2,2,2,2,2,2,2),(3,3,3,3,3,3,3,3,3,3),
       (3,3,3,3,3,3,3,3,3,3),(3,3,3,3,3,3,3,3,3,3),

       (1,1,1,1,1,1,1,1,1,1),(1,1,1,1,1,1,1,1,1,1),
       (1,1,1,1,1,1,1,1,1,1),(2,2,2,2,2,2,2,2,2,2),
       (2,2,2,2,2,2,2,2,2,2),(2,2,2,2,2,2,2,2,2,2),
       (2,2,2,2,2,2,2,2,2,2),(3,3,3,3,3,3,3,3,3,3),
       (3,3,3,3,3,3,3,3,3,3),(3,3,3,3,3,3,3,3,3,3),

       (4,4,4,4,4,4,4,4,4,4),(4,4,4,4,4,4,4,4,4,4),
       (4,4,4,4,4,4,4,4,4,4),(5,5,5,5,5,5,5,5,5,5),
       (5,5,5,5,5,5,5,5,5,5),(7,7,7,7,7,7,7,7,7,7),
       (7,7,7,7,7,7,7,7,7,7),(7,7,7,7,7,7,7,7,7,7),
       (7,7,7,7,7,7,7,7,7,7),(7,7,7,7,7,7,7,7,7,7) );

    end Sensores_Electrodos;

    --procedure Reading_Sensors (L: out Tipo_Registro)
    --    renames Sensores_Electrodos.Reading_Sensors;
    procedure Reading_Sensors (L: out EEG_Samples_Type) is
      begin
        Sensores_Electrodos.Reading_Sensors (L);
      end Reading_Sensors;


    protected body Sensores_Electrodos is
      procedure Reading_Sensors (L: out EEG_Samples_Type)  is
         type Time_index is delta 0.1 range 0.0..50.0;
         t: Time_index;
      begin
         t := Time_index(To_Duration(Clock - Big_Bang));
         i := indice_Secuencia_Sensores (integer(t * 10.0) mod 50);
         L := Secuencia(i);
         --i := i + 1;
         Execution_Time (WCET_EEG);
      end Reading_Sensors;
    end Sensores_Electrodos;


   ---------------------------------------------------------------------
   -- Procedures to get and analyze an eyes image 
   ---------------------------------------------------------------------

    cantidad_datos_EyesImage: constant := 50;
    type Indice_Secuencia_EyesImage is mod cantidad_datos_EyesImage;
    type tipo_Secuencia_EyesImage is array (Indice_Secuencia_EyesImage) of Eyes_Samples_Type;

    protected Lectura_EyesImage is
       procedure Reading_EyesImage (L: out Eyes_Samples_Type);
       procedure Reaction (Level: in integer);
    private
      i: Indice_Secuencia_EyesImage := 1;
      Secuencia: tipo_Secuencia_EyesImage :=
                ((85,85),(70,70),(85,85),(85,85),(05,05),
                 (85,85),(85,85),(20,20),(85,85),(85,85),
 
                 (70,70),(60,60),(60,60),(40,40),(40,40),
                 (10,10),(10,10),( 0, 0),( 0, 0),( 0, 0),

                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0),
                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0),

                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0),
                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0),

                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0),
                 ( 0, 0),( 0, 0),( 0, 0),( 0, 0),( 0, 0) );
    end Lectura_EyesImage;

    procedure Reading_EyesImage (L: out Eyes_Samples_Type) is
      begin
        Lectura_EyesImage.Reading_EyesImage (L);
      end Reading_EyesImage;

    protected body Lectura_EyesImage is
      procedure Reading_EyesImage (L: out Eyes_Samples_Type)  is
         type Time_index is delta 0.1 range 0.0..50.0;
         t: Time_index;
      begin
         t := Time_index(To_Duration(Clock - Big_Bang));
         i := Indice_Secuencia_eyesImage (integer(t * 10.0) mod 50);
         L := Secuencia(i);
         --i := i + 1;
         Execution_Time (WCET_Eyes_Image);
      end Reading_EyesImage;

      procedure Reaction (Level: in integer) is
         type Time_index is delta 0.1 range 0.0..50.0;
         t: Time_index;

      begin
         t := Time_index(To_Duration(Clock - Big_Bang));
         i := Indice_Secuencia_eyesImage (t * 10.0);
         if (Level = 1) then
           for k in i..i+4 loop
              secuencia (k):= (Eyes_Samples_Values(80),Eyes_Samples_Values(80));
           end loop; 
         elsif (Level = 2) then 
           for k in i..Indice_Secuencia_eyesImage'Last loop
              Secuencia (k):= (Eyes_Samples_Values(80),Eyes_Samples_Values(80)); 
           end loop;
         end if;
      end Reaction;


    end Lectura_EyesImage;

   ---------------------------------------------------------------------
   -- Procedures to access distance sensor
   ---------------------------------------------------------------------
    cantidad_datos_CarDistance: constant := 50;
    type Indice_Secuencia_CarDistance is mod cantidad_datos_CarDistance;
    type tipo_Secuencia_CarDistance is array (Indice_Secuencia_CarDistance) of Values_Car_Distance;

    protected Lectura_DistanceSensor is
       procedure Reading_CarDistance (D: out Values_Car_Distance);
    private
      i: Indice_Secuencia_CarDistance := 1;
      Secuencia: tipo_Secuencia_CarDistance :=
                ( 90, 87, 81, 77, 86, 89, 94,100,100,100,
                 100, 29, 25, 22, 19, 16, 14, 12, 11,100,
                 100,100,100, 35, 30, 24, 19, 16, 14, 12,
                  13, 14, 16, 17, 19, 21, 23, 23, 23,100,
                 100,100,100,100,100,100,100,100,100,100);
    end Lectura_DistanceSensor;

    procedure Reading_CarDistance (D: out Values_Car_Distance) is
      begin
        Lectura_DistanceSensor.Reading_CarDistance (D);
    end Reading_CarDistance;

    protected body Lectura_DistanceSensor is
      procedure Reading_CarDistance (D: out Values_Car_Distance) is
         type Time_index is delta 0.1 range 0.0..50.0;
         t: Time_index;
      begin
         t := Time_index(To_Duration(Clock - Big_Bang));
         i := Indice_Secuencia_CarDistance (integer(t * 10.0) mod 50);
         D := Secuencia(i);
         --i := i + 1;
         Execution_Time (WCET_Car_Distance);
      end Reading_CarDistance;
    end Lectura_DistanceSensor;

---------------------------------------------------------------------
--     Cuerpo de los procedmientos y objetos para DISPOSITIVOS E/S 
---------------------------------------------------------------------

procedure Display_Pulse_Rate (P: Values_Pulse_Rate) is
begin
   Current_Time (Big_Bang);
   Put ("............# ");
   Put ("Pulse Rate: ");
   Print_an_Integer (Integer(P));
   Execution_Time (WCET_Display);
end Display_Pulse_Rate;


procedure Display_Electrodes_Sample (R: EEG_Samples_Type) is
begin
   Current_Time (Big_Bang);
   Put ("............# ");
   Put ("Electrodes Values: ");
   for i in EEG_Samples_Index loop
      Print_an_Integer (Integer(R(i)));
   end loop;
   Execution_Time (WCET_Display);
end Display_Electrodes_Sample;


procedure Display_Eyes_Sample (R: Eyes_Samples_Type) is
 Average: Eyes_Samples_Values;
begin
   Current_Time (Big_Bang);
   Put ("............# ");
   Put ("Eyes Openness: ");
   for i in Eyes_Samples_Index loop
      Print_an_Integer (Integer(R(i)));
   end loop;

   Average := (R(Right) + R(Left))/2;
   if Average > 80 then    Put ("   (O,O)");
   elsif Average > 60 then Put ("   (o,o)");
   elsif Average > 30 then Put ("   (*,*)");
   else                    Put ("   (-,-)");
   end if;

   Execution_Time (WCET_Display);
end Display_Eyes_Sample;

procedure Display_Car_Distance (D: Values_Car_Distance) is
begin
   Current_Time (Big_Bang);
   Put ("............# ");
   Put ("Car Distance: ");
   Print_an_Integer (Integer(D));

   Execution_Time (WCET_Display);
end Display_Car_Distance;


procedure Display_Cronometro (Origen : Ada.Real_Time.Time; Hora: Ada.Real_Time.Time ) is
  type Crono is delta 0.1 range 0.0..100.0;
begin
  Current_Time (Big_Bang);
  Put ("............%Crono:");
  --Put (Duration'Image(To_Duration(Clock - Origen)));
  Put (Crono'Image(Crono(To_Duration(Hora - Origen))));
end Display_Cronometro;



procedure Light (E: Light_States) is
begin
   Current_Time (Big_Bang);
   case E is
        when On  => Put ("............Light: ^ON^");
        when Off => Put ("............Light: _off_");
   end case;
   Execution_Time (WCET_Light);
end Light;



procedure Beep (v: Volume) is 
    -- emite un sonido durante 0.5 segundos con volumne "v"
begin
  Current_Time (Big_Bang);

  Put ("............%B");
  for i in 1..v loop
    Put ("EE");
  end loop ;  
  Put ("P");
  Put (Volume'Image(v));
  Execution_Time (WCET_Alarm);
  Lectura_EyesImage.Reaction (EYES_REACTION_WHEN_BEEP);
end Beep;


procedure Activate_Automatic_Driving is
begin
   Current_Time (Big_Bang);
   Put ("!!!! Automatic driving system activated !!!!");
   Execution_Time (WCET_Automatic_Driving);
end Activate_Automatic_Driving;


---------------------------------------------------------------------------------------
begin
   null;
end devices;


