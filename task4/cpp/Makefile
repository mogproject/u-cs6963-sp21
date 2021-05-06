MKDIR = mkdir
CP = cp
RM = rm
MV = mv
EXEC = Exact

build:
	cmake -S . -B build/Release -DCMAKE_BUILD_TYPE=Release
	cmake --build build/Release
	cp build/Release/$(EXEC) dist/
	@echo "Created: dist/$(EXEC)"
	@echo "Testing..."
	../scripts/bulk_test.py ../data/small/graph5.dat --exe dist/$(EXEC)

clean:
	@echo "Cleaning..."
	@$(RM) -rf build/*
	@$(RM) -rf dist/*
	@echo "Cleaning done."

test:
	cmake -DBUILD_TESTS=ON -S . -B build/Debug
	cmake --build build/Debug
	cd build/Debug/test && ctest --output-on-failure

publish:
	$(MV) -f CMakeLists.txt CMakeLists.txt.bk
	$(CP) -f CMakeLists_submit.txt CMakeLists.txt
	$(MKDIR) -p dist
	tar zcvf dist/$(EXEC).tgz CMakeLists.txt src
	$(MV) -f CMakeLists.txt.bk CMakeLists.txt
	@echo "Created: dist/$(EXEC).tgz"

.PHONY: build clean test publish
