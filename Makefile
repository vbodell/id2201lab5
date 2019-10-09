compile:
	@printf "c(test).\nc(node1).\nc(key).\nhalt()." > compile
	erl < compile
	@rm compile

test2: compile
	@echo "=======TEST2======"
	erl < test2
test: compile
	@echo "=======TEST======"
	erl < test
test3: compile
	@echo "=======TEST3======"
	erl < test3

submit: 
	zip submit.zip *.erl ID2201_* Makefile

clean:
	@rm *.beam
