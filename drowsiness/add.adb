
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
with Ada.Interrupts.Names;
with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    -----------------------------------------------------------------------
    ------------- Types
    -----------------------------------------------------------------------
    type e_state is new Integer range 0..2;
    type eeg_state_type is new Natural range 0..1;
    estados_luz: constant:= 2;
    type dos is mod estados_luz;

    -----------------------------------------------------------------------
    ------------- declaration of protected data
    -----------------------------------------------------------------------
    protected Eyes_state is
        pragma priority (System.Priority'First + 6);
        function get_r_eyes return Eyes_Samples_Type;
        procedure set_r_eyes (r: Eyes_Samples_Type);
        function get_eyes_state return e_state;
        procedure set_eyes_state (s: e_state);
    private 
            R_eyes: Eyes_Samples_Type := (0, 0);
            state: e_state:= 0;
    end Eyes_state;

    protected EEG_state is
        pragma priority (System.Priority'First + 8);
        function get_r_eeg return EEG_Samples_Type;
        procedure set_r_eeg (r: EEG_Samples_Type);
        function get_eeg_state return eeg_state_type;
        procedure set_eeg_state (s: eeg_state_type) ;
        function get_pulse return Values_Pulse_Rate;
        procedure set_pulse (P: Values_Pulse_Rate);
    private 
            R_eeg: EEG_Samples_Type := (others=>0);
            egg_state: eeg_state_type := 0;
            P_pulse_rate: Values_Pulse_Rate := 20.0;
    end EEG_state;

    protected int_handler is
        pragma Priority (Priority_Of_External_Interrupts_2);
        procedure Handler;
        pragma Attach_Handler (Handler , Ada.Interrupts.Names.External_Interrupt_2);
        entry Esperar_Evento;
    private
        Llamada_Pendiente : Boolean := False; --barrera
    end int_handler;    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    task Electrodes is 
    pragma priority (System.Priority'First + 7);
    end Electrodes;

    task Eyes_Detection is 
    pragma priority (System.Priority'First + 6);
    end Eyes_Detection; 

    task Show_info is
    pragma priority (System.Priority'First + 3);
    end Show_info;

    task Pulse_measuring is
    pragma priority (System.Priority'First + 8);
    end Pulse_measuring;

    task Risk_Control is
    pragma priority (System.Priority'First + 4);
    end Risk_Control;

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

        function get_eyes_state return e_state is
            begin
                return state;
        end get_eyes_state;

        procedure set_eyes_state (s: e_state) is
            begin
                state := s;
        end set_eyes_state;

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

        function get_eeg_state return eeg_state_type is
            begin
                return egg_state;
        end get_eeg_state;

        procedure set_eeg_state (s: eeg_state_type) is
            begin
                egg_state := s;
        end set_eeg_state;

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
        end set_pulse;
    end EEG_state;

    protected body int_handler is 
        procedure Handler is
        begin
            Llamada_Pendiente := True;
        end Handler;

        entry Esperar_Evento when Llamada_Pendiente is
        begin  
            Llamada_Pendiente := False;
        end Esperar_Evento;
     end int_handler;

    ----------------------------------------------------------------------
    task body Electrodes  is 
        R: EEG_Samples_Type;
        next_time: Time := big_bang;
        sum: Natural:= 0;
        period : constant Time_Span := Milliseconds (300);
    begin
        next_time:= next_time + period;
        loop
            --Starting_Notice ("Start_Electrodes"); 
            Reading_Sensors (R);
            EEG_state.set_r_eeg(R);
            sum := 0;
            for i in 7..10 loop
                sum := sum + Natural(R(EEG_Samples_Index(i)));
            end loop;
            if ( sum < 20 ) then 
                EEG_state.set_eeg_state(eeg_state_type(0));
            else
                EEG_state.set_eeg_state(eeg_state_type(1));
            end if;
            --Finishing_Notice ("Finish_Electrodes");
            delay until next_time;
            next_time:= next_time + period;
        end loop;
        
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Current_R: Eyes_Samples_Type;
        next_time: Time := big_bang;
        counter: Integer := 0;
        period : constant Time_Span := Milliseconds (150);
    begin
        next_time:= next_time + period;
        loop
            --Starting_Notice ("Start_Eyes_Detection");
            Reading_EyesImage (Current_R);
            Eyes_state.set_r_eyes(Current_R);

            if (Current_R(left) < 20 and Current_R(right) < 20 ) then
                Counter := Counter + 1;
                if (counter = 2) then 
                    Eyes_state.set_eyes_state(1);
                elsif (counter > 3 ) then
                    Eyes_state.set_eyes_state(2);
                end if;
            else 
                Counter := 0;
                Eyes_state.set_eyes_state(0);
            end if;               
            
            --Finishing_Notice ("Finish_Eyes_Detection");
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
            delay until next_time;
            --Starting_Notice ("Start_Show_Info");
            R_eyes := Eyes_state.get_r_eyes;
            R_eeg := EEG_state.get_r_eeg;
            Pulse := EEG_state.get_pulse;
            Display_Eyes_Sample (R_eyes);
            Display_Electrodes_Sample(R_eeg);
            Display_Pulse_Rate (Pulse);
            --Finishing_Notice ("Finish_Info");

            next_time:= next_time + period;
        end loop;

    end Show_info;

    ---------------------------------------------------------------------
    task body Pulse_measuring is
        pulse : float;
        last_time: time := big_bang;
        now : time;
        time_lapsed: Time_Span;
    begin
        loop
            int_handler.Esperar_Evento;
            --Starting_Notice ("Start_Pulse_measuring");
            now:= clock;
            time_lapsed := now - last_time;
            pulse := float(to_duration(time_lapsed));

            if(pulse < 0.2) then
                pulse := 300.0;
            else
                pulse := 60.0 / pulse;
            end if;

            EEG_state.set_pulse (Values_Pulse_Rate(pulse));            
            last_time := now;
            --Finishing_Notice ("Finish_Pulse_measuring");

        end loop;

    end Pulse_measuring;

    task body Risk_Control is
        R_eyes: Eyes_Samples_Type;
        R_eeg: EEG_Samples_Type;
        eyes: e_state; 
        atention: eeg_state_type := 0;
        Pulse: Values_Pulse_Rate;
        Sign_Counter: Integer := 0;
        blink_counter: dos := 1;
        light_state: Light_States := OFF;
        next_time: Time := big_bang;
        period : constant Time_Span := Milliseconds (250);
    begin
        next_time:= next_time + period;
        loop
            delay until next_time;
            Starting_Notice ("Start_Risk_control");

            R_eyes := Eyes_state.get_r_eyes;
            R_eeg := EEG_state.get_r_eeg;
            eyes:= Eyes_state.get_eyes_state;
            atention := EEG_state.get_eeg_state;
            Pulse := EEG_state.get_pulse;
            Display_Eyes_Sample (R_eyes);
            Display_Electrodes_Sample(R_eeg);
            Display_Pulse_Rate (Pulse);
            
            -------Contador de Sintomas
            if  (eyes > 0 ) then 
                Sign_Counter := Sign_Counter + 1;
            end if;
            if ( atention = 0 ) then
                Sign_Counter := Sign_Counter + 1;
            end if;
            if (Pulse <= 50.0) then
                Sign_Counter := Sign_Counter + 1;
            end if;
            -----------------------------
            --------Acciones del sistema
            if (Sign_Counter = 1) then
                if (eyes = 1 ) then
                    Beep(2);
                elsif (eyes = 2 )then
                    Beep(3);
                end if;

                if ( atention = 0 ) then
                    light_state := ON;
                else 
                    light_state := OFF;
                end if;

            elsif (Sign_Counter = 2) then
                Beep(4);
                if (blink_counter = 1) then
                    if (light_state = ON) then
                        light_state := OFF;
                    else
                        light_state := ON;
                    end if;
                end if;

                blink_counter := dos((blink_counter + 1));

            elsif (Sign_Counter >= 3) then
                Activate_Automatic_Driving;
                Beep(5);
                 if (blink_counter = 1) then
                    if (light_state = ON) then
                        light_state := OFF;
                    else
                        light_state := ON;
                    end if;
                end if;
                blink_counter := dos((blink_counter + 1));

            else 
                light_state := OFF;
            end if;  

            if ( Sign_Counter < 2 ) then 
                blink_counter := 1;
            end if;
            light(light_state);

            Sign_Counter := 0;            
            Finishing_Notice ("Finish_Risk_control");

            next_time:= next_time + period;
        end loop;

    end Risk_Control;

begin
   null;
end add;
