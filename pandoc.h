#define PANDOC_FMT_MARKDOWN      0
#define PANDOC_FMT_RST           1
#define PANDOC_FMT_HTML          2
#define PANDOC_FMT_LATEX         3
#define PANDOC_FMT_S5            4
#define PANDOC_FMT_DOCBOOK       5
#define PANDOC_FMT_ODT           6
#define PANDOC_FMT_CONTEXT       7
#define PANDOC_FMT_TEXINFO       8
#define PANDOC_FMT_MAN           9
#define PANDOC_FMT_MEDIAWIKI     10
#define PANDOC_FMT_RTF           11

char* pandoc(int input_format, 
	     char* parser_state_json,
	     int output_format,
	     char* writer_options_json,
	     char* text);

