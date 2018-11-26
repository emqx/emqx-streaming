{application, emqx-streaming, [
	{description, "EMQ X Streaming"},
	{vsn, "0.0.1"},
	{modules, ['emqx_streaming','emqx_streaming_app','emqx_streaming_sup']},
	{registered, []},
	{applications, [kernel,stdlib]}
]}.