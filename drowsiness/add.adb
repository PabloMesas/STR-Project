
--------------------------------------------
-----             STR 2018             -----
-----       Pablo Mesas Lafarga        -----
-----    Alejandro Mendez Fernandez    -----
--------------------------------------------

-- t- eyes                  -- OP-Eyes
-- t- EEG                   -- OP-EEG
-- t- ShowInfo              --OP-Pulse
-- t- Pulse
-- +RTI (Work in Progress)


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
    ------------- declaration of protected data
    -----------------------------------------------------------------------
    protected Eyes_state is
        function get_r_eyes return Eyes_Samples_Type;
        procedure set_r_eyes (r: Eyes_Samples_Type);
    private 
            R_eyes: Eyes_Samples_Type := (0, 0);
    end Eyes_state;

    protected EEG_state is
        function get_r_eeg return EEG_Samples_Type;
        procedure set_r_eeg (r: EEG_Samples_Type);
        function get_pulse return Values_Pulse_Rate;
        procedure set_pulse (P: Values_Pulse_Rate);
    private 
            R_eeg: EEG_Samples_Type := (others=>0);
            P_pulse_rate: Values_Pulse_Rate := 20.0;
    end EEG_state;

    -- protected int_handler is
    --     procedure Handler;
    --     pragma Interrupt_Handler (Handler);
    --     private
    -- end int_handler;
    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    task Electrodes is 
    pragma priority (System.Priority'First + 11);
    end Electrodes;

    task Eyes_Detection is 
    pragma priority (System.Priority'First + 10);
    end Eyes_Detection; 

    task Show_info is
    pragma priority (System.Priority'First + 8);
    end Show_info;

    task Pulse_measuring is
    pragma priority (System.Priority'First + 12);
    end Pulse_measuring;

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    -----------------------------------------------------------------------
    ------------- definition of protected data
    -----------------------------------------------------------------------

    protected body Eyes_state is
        function get_r_eyes return Eyes_Samples_Type is
            begin
                return R_eyes;
        end get_r_eyes;

        procedure set_r_eyes (r: Eyes_Samples_Type) is
            begin
                R_eyes := r;
        end set_r_eyes;
    end Eyes_state;

    protected body EEG_state is
        function get_r_eeg return EEG_Samples_Type is
            begin
                return R_eeg;
        end get_r_eeg;

        procedure set_r_eeg (r: EEG_Samples_Type) is
            begin
                R_eeg := r;
        end set_r_eeg;

        function get_pulse return Values_Pulse_Rate is
            begin
                return P_pulse_rate;
        end get_pulse;

        procedure set_pulse (P: Values_Pulse_Rate) is
            begin
                if(P >= 20.0) then
                    if(P > 300.0) then
                        P_pulse_rate := 300.0;
                    else
                        P_pulse_rate := P;
                    end if;  
                else
                    P_pulse_rate := 20.0;
                end if;
                Print_an_Integer (Integer(P));
        end set_pulse;
    end EEG_state;

    -- protected body int_handler is 
    --     procedure Handler is
    --         begin

    --     end Handler;
    -- end int_handler;

    ----------------------------------------------------------------------
    task body Electrodes  is 
        R: EEG_Samples_Type;
        next_time: Time := big_bang;
        period : constant Time_Span := Milliseconds (300);
        sum: Natural:= 0;
    begin
        next_time:= next_time + period;
        loop
            -- Starting_Notice ("Start_Electrodes"); 
            Reading_Sensors (R);
            EEG_state.set_r_eeg(R);
            sum := 0;
            for i in 7..10 loop
                sum := sum + Natural(R(EEG_Samples_Index(i)));
            end loop;
            if ( sum < 20 ) then
                light(ON);
            else 
                light(OFF);
            end if;
            -- Finishing_Notice ("Finish_Electrodes");

            delay until next_time;
            next_time:= next_time + period;
        end loop;
        
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Counter: Integer := 0;
        Current_R: Eyes_Samples_Type;
        next_time: Time := big_bang;
        period : constant Time_Span := Milliseconds (150);
    begin
        next_time:= next_time + period;
        loop
            -- Starting_Notice ("Start_Eyes_Detection");
            Reading_EyesImage (Current_R);
            Eyes_state.set_r_eyes(Current_R);
            if (Current_R(left) < 20 and Current_R(right) < 20 ) then
                Counter := Counter + 1;
            else Counter := 0;
            end if;

            if (Counter = 2 ) then
                Beep(2);
            elsif (Counter > 3 )then
                Beep(3);
            end if;
            -- Finishing_Notice ("Finish_Eyes_Detection");

            delay until next_time;
            next_time:= next_time + period;
        end loop;

    end Eyes_Detection;

    ---------------------------------------------------------------------
    task body Show_info is
        R_eyes: Eyes_Samples_Type;
        R_eeg: EEG_Samples_Type;
        Pulse: Values_Pulse_Rate;
        next_time: Time := big_bang;
        period : constant Time_Span := Milliseconds (1000);
    begin
        next_time:= next_time + period;
        loop
            -- Starting_Notice ("Start_Show_Info");
            R_eyes := Eyes_state.get_r_eyes;
            R_eeg := EEG_state.get_r_eeg;
            Pulse := EEG_state.get_pulse;
            -- Display_Eyes_Sample (R_eyes);
            -- Display_Electrodes_Sample(R_eeg);
            -- Display_Pulse_Rate (Pulse);
            -- Finishing_Notice ("Finish_Info");

            delay until next_time;
            next_time:= next_time + period;
        end loop;

    end Show_info;

    ---------------------------------------------------------------------
    task body Pulse_measuring is
        pulse : float;
        last_time: time := big_bang;
        now : time;
        time_lapsed: Time_Span;
        next_time: Time := big_bang;
        period : constant Time_Span := Milliseconds (3000);
    begin
        next_time := next_time + period;
        loop
            now:= clock;
            time_lapsed := now - last_time;

            Starting_Notice ("Start_Pulse_measuring");

            pulse := float(to_duration(time_lapsed));
            if(pulse < 0.2) then
                pulse := 300.0;
            else
                pulse := 60.0 / pulse;
            end if;

            EEG_state.set_pulse (Values_Pulse_Rate(pulse));

            Finishing_Notice ("Finish_Pulse_measuring");
            
            last_time := now;
            delay until next_time;
            next_time:= next_time + period;

        end loop;

    end Pulse_measuring;



begin
   null;
end add;





