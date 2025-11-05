#include "orilla-compiler.h"

int main(const int argc, char const *argv[]) {
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <filename> [-o output]" << std::endl;
    exit(EXIT_FAILURE);
  }

  std::string executableName = "output";
  char const *filePath = nullptr;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-o" && i + 1 < argc) {
      executableName = argv[++i];
    } else if (filePath == nullptr) {
      filePath = argv[i];
    } else {
      std::cerr << "Unknown argument: " << arg << std::endl;
      std::cerr << "Usage: " << argv[0] << " <filename> [-o output]"
                << std::endl;
      exit(EXIT_FAILURE);
    }
  }

  if (filePath == nullptr) {
    std::cerr << "Error: No input file specified" << std::endl;
    std::cerr << "Usage: " << argv[0] << " <filename> [-o output]" << std::endl;
    exit(EXIT_FAILURE);
  }
  std::ifstream file(filePath);
  if (!file.is_open()) {
    std::cerr << "Error: Could not open file " << filePath << std::endl;
    exit(EXIT_FAILURE);
  }

  std::ostringstream ss;
  ss << file.rdbuf();
  file.close();

  std::vector<Token> tokens = tokenize(ss.str());

  std::optional<NodeProg> root = parse(tokens);

  if (!root.has_value()) {
    std::cerr << "Error: Could not parse file " << filePath << std::endl;
    exit(EXIT_FAILURE);
  }

  std::string outputPath = executableName + ".o";

  bool generated = generate(std::move(root.value()), outputPath);

  if (!generated) {
    std::cerr << "Error: Could not generate output file." << std::endl;
    exit(EXIT_FAILURE);
  }

  std::string compileCmd = "clang " + outputPath + " -o " + executableName;
  std::system(compileCmd.c_str());

  std::filesystem::remove(outputPath);

  return 0;
}
