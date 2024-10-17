#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

#define MAX_FILES 50
#define COMMAND_BUFFER_SIZE 1024
#define LOG_FILE "compilation_log.txt"
#define MAX_LOG_LINES 100

typedef struct CompilationContext {
    char commandBuffer[COMMAND_BUFFER_SIZE];
    const char *sourceFileNames[MAX_FILES];
    int numberOfFiles;
    int compilationSuccessFlag;
    const char *outputExecutableName;
    FILE *logFile;
    char logMessages[MAX_LOG_LINES][COMMAND_BUFFER_SIZE];
    int logIndex;
} CompilationContext;

void initialize_compilation_context(CompilationContext *context, int fileCount, char *files[], const char *outputFile) {
    context->numberOfFiles = fileCount;
    for (int i = 0; i < fileCount; ++i) {
        context->sourceFileNames[i] = files[i];
    }
    context->outputExecutableName = outputFile ? outputFile : "output_program.exe";
    context->compilationSuccessFlag = -1;
    memset(context->commandBuffer, 0, sizeof(context->commandBuffer));
    context->logIndex = 0;

    context->logFile = fopen(LOG_FILE, "w");
    if (context->logFile == NULL) {
        fprintf(stderr, "Error opening log file.\n");
        exit(EXIT_FAILURE);
    }
}

void log_message(CompilationContext *context, const char *message) {
    if (context->logIndex < MAX_LOG_LINES) {
        snprintf(context->logMessages[context->logIndex++], COMMAND_BUFFER_SIZE, "%s\n", message);
        fprintf(context->logFile, "%s\n", message);
    } else {
        fprintf(stderr, "Log limit reached. Unable to log: %s\n", message);
    }
}

void generate_compile_command(CompilationContext *context) {
    snprintf(context->commandBuffer, sizeof(context->commandBuffer), "gcc -o %s", context->outputExecutableName);
    for (int i = 0; i < context->numberOfFiles; ++i) {
        strcat(context->commandBuffer, " ");
        strcat(context->commandBuffer, context->sourceFileNames[i]);
    }
    log_message(context, context->commandBuffer);
}

void execute_compilation_and_run(CompilationContext *context) {
    context->compilationSuccessFlag = system(context->commandBuffer);
    if (context->compilationSuccessFlag == 0) {
        log_message(context, "Compilation successful. Running the program...");
        printf("Compilation successful. Running the program...\n");
        system(context->outputExecutableName);
    } else {
        log_message(context, "Compilation failed with error code: %d");
        printf("Compilation failed with error code: %d\n", context->compilationSuccessFlag);
    }
}

void cleanup_compilation_context(CompilationContext *context) {
    memset(context->commandBuffer, 0, sizeof(context->commandBuffer));
    for (int i = 0; i < context->numberOfFiles; ++i) {
        context->sourceFileNames[i] = NULL;
    }
    context->numberOfFiles = 0;
    context->compilationSuccessFlag = -1;
    fclose(context->logFile);
}

void print_usage_message(const char *programName) {
    fprintf(stderr, "Usage: %s <file1.c> [file2.c ... fileN.c] [-o output_file]\n", programName);
}

void handle_error(const char *errorMessage) {
    fprintf(stderr, "Error: %s\n", errorMessage);
    exit(EXIT_FAILURE);
}

int is_valid_source_file(const char *filename) {
    return strstr(filename, ".c") != NULL;
}

void list_source_files(const char *directory, CompilationContext *context) {
    struct dirent *entry;
    DIR *dp = opendir(directory);
    if (dp == NULL) {
        handle_error("Could not open directory.");
    }

    while ((entry = readdir(dp))) {
        if (is_valid_source_file(entry->d_name)) {
            if (context->numberOfFiles < MAX_FILES) {
                context->sourceFileNames[context->numberOfFiles++] = entry->d_name;
                char message[COMMAND_BUFFER_SIZE];
                snprintf(message, sizeof(message), "Found source file: %s", entry->d_name);
                log_message(context, message);
            } else {
                fprintf(stderr, "Max file limit reached. Skipping file: %s\n", entry->d_name);
            }
        }
    }
    closedir(dp);
}

void set_output_file_from_args(CompilationContext *context, int argc, char *argv[]) {
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            context->outputExecutableName = argv[i + 1];
            log_message(context, "Output file set to:");
            log_message(context, context->outputExecutableName);
            break;
        }
    }
}

int main(int argc, char *argv[]) {
    CompilationContext compilationContext;

    if (argc < 2) {
        print_usage_message(argv[0]);
        return 1;
    }

    set_output_file_from_args(&compilationContext, argc, argv);

    int fileCount = 0;
    if (argc == 2) {
        list_source_files(".", &compilationContext);
    } else {
        for (int i = 1; i < argc; ++i) {
            if (is_valid_source_file(argv[i])) {
                compilationContext.sourceFileNames[fileCount++] = argv[i];
                char message[COMMAND_BUFFER_SIZE];
                snprintf(message, sizeof(message), "Adding source file: %s", argv[i]);
                log_message(&compilationContext, message);
            }
        }
    }
    
    initialize_compilation_context(&compilationContext, fileCount, compilationContext.sourceFileNames, compilationContext.outputExecutableName);
    generate_compile_command(&compilationContext);
    execute_compilation_and_run(&compilationContext);
    cleanup_compilation_context(&compilationContext);

    return 0;
}
