override SRC		=	bin/typechecker.ml	\
				bin/compiler.ml

override SRC_MAIN	=	bin/minic.ml

override SRC_TESTS	=	tests/test_lib.ml	\
				tests/test_tchk.ml

override BLOB		=	lib/C_AST.cmx		\
				lib/lexer.cmx		\
				lib/parser.cmx		\
				lib/runner.cmx


override BUILD_DIR	=	_build

override OBJ		=	$(SRC:%.ml=$(BUILD_DIR)/%.o)
override CMX		=	$(SRC:%.ml=$(BUILD_DIR)/%.cmx)
override CMI		=	$(SRC:%.ml=$(BUILD_DIR)/%.cmi)

override OBJ_MAIN	=	$(SRC_MAIN:%.ml=$(BUILD_DIR)/%.o)
override CMX_MAIN	=	$(SRC_MAIN:%.ml=$(BUILD_DIR)/%.cmx)
override CMI_MAIN	=	$(SRC_MAIN:%.ml=$(BUILD_DIR)/%.cmi)

override OBJ_TESTS	=	$(SRC_TESTS:%.ml=$(BUILD_DIR)/%.o)
override CMX_TESTS	=	$(SRC_TESTS:%.ml=$(BUILD_DIR)/%.cmx)
override CMI_TESTS	=	$(SRC_TESTS:%.ml=$(BUILD_DIR)/%.cmi)

override CP		?=	cp
override OCAMLOPT	?=	ocamlopt
override OCAMLOPTFLAGS	+=	-I $(BUILD_DIR)/bin -I $(BUILD_DIR)/lib -I $(BUILD_DIR)/tests
override RMFLAGS	+=	-r

override NAME		?=	minic
override NAME_TESTS	?=	run_tests

override BLOB_OBJ	=	$(BLOB:%.cmx=$(BUILD_DIR)/%.cmx) $(CMX) $(CMX_MAIN)
override BLOB_TESTS_OBJ	=	$(BLOB:%.cmx=$(BUILD_DIR)/%.cmx) $(CMX) $(CMX_TESTS)


all:			clean $(NAME) $(NAME_TESTS)

$(NAME):		$(OBJ) $(OBJ_MAIN)
			$(OCAMLOPT) $(BLOB_OBJ) -o $@

$(NAME_TESTS):		$(OBJ) $(OBJ_TESTS)
			$(OCAMLOPT) $(BLOB_TESTS_OBJ) -o $@

$(BUILD_DIR)/%.o:	$(BUILD_DIR)/%.ml
			$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -o $@

$(BUILD_DIR)/%.ml:	%.ml
			@$(CP) $< $@

clean:
			$(RM) $(RMFLAGS) $(NAME) $(NAME_TESTS)
			$(RM) $(RMFLAGS) $(OBJ) $(OBJ_MAIN) $(OBJ_TESTS)
			$(RM) $(RMFLAGS) $(CMX) $(CMX_MAIN) $(CMX_TESTS)
			$(RM) $(RMFLAGS) $(CMI) $(CMI_MAIN) $(CMI_TESTS)

.NOTPARALLEL:
.PHONY:			all clean
