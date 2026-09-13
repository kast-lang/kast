impl Float64 as module = (
    module:
    const to_bits = (self :: Float64) -> UInt64 => @cfg (
        | target.name == "interpreter" => (@native "float64.to_bits")(self)
        | target.name == "c" => @native "Float64_to_bits(\(self))"
        | target.name == "javascript" => (@native "Kast.Char.string_encoding_len")(self)
    );
);
