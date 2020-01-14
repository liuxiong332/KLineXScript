
-- VHDL Instantiation Created from source file ID_ALU_regs.vhd -- 14:14:31 11/22/2017
--
-- Notes: 
-- 1) This instantiation template has been automatically generated using types
-- std_logic and std_logic_vector for the ports of the instantiated module
-- 2) To use this template to instantiate this entity, cut-and-paste and then edit

	COMPONENT ID_ALU_regs
	PORT(
		clk : IN std_logic;
		rst : IN std_logic;
		bubble : IN std_logic;
		stall : IN std_logic;
		pc_in : IN std_logic_vector(15 downto 0);
		wb_src_in : IN std_logic_vector(2 downto 0);
		mem_data_from_reg_in : IN std_logic_vector(15 downto 0);
		wb_data_from_reg_in : IN std_logic_vector(15 downto 0);
		immediate_in : IN std_logic_vector(15 downto 0);
		write_back_reg_in : IN std_logic_vector(3 downto 0);
		reg_write_enable_in : IN std_logic;
		op_code_in : IN std_logic_vector(3 downto 0);
		operand1_in : IN std_logic_vector(15 downto 0);
		operand2_in : IN std_logic_vector(15 downto 0);
		cin_in : IN std_logic;
		mem_enable_in : IN std_logic;
		mem_read_in : IN std_logic;
		mem_write_in : IN std_logic;          
		pc_out : OUT std_logic_vector(15 downto 0);
		wb_src_out : OUT std_logic_vector(2 downto 0);
		mem_data_from_reg_out : OUT std_logic_vector(15 downto 0);
		wb_data_from_reg_out : OUT std_logic_vector(15 downto 0);
		immediate_out : OUT std_logic_vector(15 downto 0);
		write_back_reg_out : OUT std_logic_vector(3 downto 0);
		reg_write_enable_out : OUT std_logic;
		op_code_out : OUT std_logic_vector(3 downto 0);
		operand1_out : OUT std_logic_vector(15 downto 0);
		operand2_out : OUT std_logic_vector(15 downto 0);
		cin_out : OUT std_logic;
		mem_enable_out : OUT std_logic;
		mem_read_out : OUT std_logic;
		mem_write_out : OUT std_logic
		);
	END COMPONENT;

	Inst_ID_ALU_regs: ID_ALU_regs PORT MAP(
		clk => ,
		rst => ,
		bubble => ,
		stall => ,
		pc_in => ,
		pc_out => ,
		wb_src_in => ,
		wb_src_out => ,
		mem_data_from_reg_in => ,
		mem_data_from_reg_out => ,
		wb_data_from_reg_in => ,
		wb_data_from_reg_out => ,
		immediate_in => ,
		immediate_out => ,
		write_back_reg_in => ,
		write_back_reg_out => ,
		reg_write_enable_in => ,
		reg_write_enable_out => ,
		op_code_in => ,
		op_code_out => ,
		operand1_in => ,
		operand1_out => ,
		operand2_in => ,
		operand2_out => ,
		cin_in => ,
		cin_out => ,
		mem_enable_in => ,
		mem_enable_out => ,
		mem_read_in => ,
		mem_read_out => ,
		mem_write_in => ,
		mem_write_out => 
	);

