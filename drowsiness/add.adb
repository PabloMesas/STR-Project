
--------------------------------------------
-----             STR 2018             -----
-----       Pablo Mesas Lafarga        -----
-----    Alejandro Mendez Fernandez    -----
--------------------------------------------

with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;
--with Ada.Calendar; use Ada.Calendar;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    task Electrodes is 
    pragma priority (System.Priority'First + 10);
    end Electrodes;

    task Eyes_Detection is 
    pragma priority (System.Priority'First + 10);
    end Eyes_Detection; 

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    ----------------------------------------------------------------------
    task body Electrodes  is 
        R: EEG_Samples_Type;
        next_time: Time;
        period : constant Time_Span := Milliseconds (300);
        sum: Natural:= 0;
    begin
        next_time:= clock + period;
        loop
            Starting_Notice ("Start_Electrodes"); 
            Reading_Sensors (R);
            sum := 0;
            for i in 7..10 loop
                sum := sum + Natural(R(EEG_Samples_Index(i)));
            end loop;
            if ( sum < 20 ) then
                light(ON);
            else 
                light(OFF);
            end if;
            Display_Electrodes_Sample (R);
            Finishing_Notice ("Finish_Electrodes");

            delay until next_time;
            next_time:= next_time + period;
        end loop;
        
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Counter: Integer := 0;
        Current_R: Eyes_Samples_Type;
        next_time: Time;
        period : constant Time_Span := Milliseconds (150);
    begin
        next_time := clock + period;
        loop
            Starting_Notice ("Start_Eyes_Detection");
            Reading_EyesImage (Current_R);
            if (Current_R(left) < 20 and Current_R(right) < 20 ) then
                Counter := Counter + 1;
            else Counter := 0;
            end if;

            if (Counter = 2 ) then
                Beep(2);
            elsif (Counter > 3 )then
                Beep(3);
            end if;

            Display_Eyes_Sample (Current_R);
            Finishing_Notice ("Finish_Eyes_Detection");

            delay until next_time;
            next_time:= next_time + period;
        end loop;

    end Eyes_Detection;


begin
   null;
end add;



