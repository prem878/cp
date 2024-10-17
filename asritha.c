#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct CompilationContext {
    char commandBuffer[512];
    const char *sourceFileName;
    int compilationSuccessFlag;
    const char *outputExecutableName;
} CompilationContext;

void initialize_compilation_context(CompilationContext *context, const char *sourceFile) {
    context->sourceFileName = sourceFile;
    context->outputExecutableName = "output_program.exe";
    context->compilationSuccessFlag = -1;
    memset(context->commandBuffer, 0, sizeof(context->commandBuffer));
}

void generate_compile_command(CompilationContext *context) {
    snprintf(context->commandBuffer, sizeof(context->commandBuffer), "gcc -o %s %s", context->outputExecutableName, context->sourceFileName);
}

void execute_compilation_and_run(CompilationContext *context) {
    context->compilationSuccessFlag = system(context->commandBuffer);
    if (context->compilationSuccessFlag == 0) {
        printf("Compilation successful. Running the program...\n");
        system(context->outputExecutableName);
    } else {
        printf("Compilation failed with error code: %d\n", context->compilationSuccessFlag);
    }
}

void cleanup_compilation_context(CompilationContext *context) {
    memset(context->commandBuffer, 0, sizeof(context->commandBuffer));
    context->sourceFileName = NULL;
    context->outputExecutableName = NULL;
    context->compilationSuccessFlag = -1;
}

void print_usage_message(const char *programName) {
    fprintf(stderr, "Usage: %s <file.c>\n", programName);
}

void handle_error(const char *errorMessage) {
    fprintf(stderr, "Error: %s\n", errorMessage);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    CompilationContext compilationContext;

    if (argc != 2) {
        print_usage_message(argv[0]);
        return 1;
    }

    initialize_compilation_context(&compilationContext, argv[1]);
    generate_compile_command(&compilationContext);
    execute_compilation_and_run(&compilationContext);
    cleanup_compilation_context(&compilationContext);

    return 0;
}
