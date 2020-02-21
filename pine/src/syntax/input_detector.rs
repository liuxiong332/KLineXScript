pub trait InputSrcDetector<'a> {
    fn get_client_srcs(&self) -> Vec<&'a str>;

    fn get_input_srcs(&self) -> Vec<&'a str>;

    // Check if the name src is input source for client.
    // Map the original source to client source name.
    fn map_client_src<'b>(&self, src: &'b str) -> Option<&'b str>;

    // Check if the src is input source and map to another source name.
    // Map the original source to input source name.
    fn map_input_src<'b>(&self, src: &'b str) -> Option<&'b str>;
}

pub struct SimpleInputSrcDetector<'a> {
    srcs: Vec<&'a str>,
}

impl<'a> SimpleInputSrcDetector<'a> {
    pub fn new(srcs: Vec<&'a str>) -> SimpleInputSrcDetector<'a> {
        SimpleInputSrcDetector { srcs }
    }
}

impl<'a> InputSrcDetector<'a> for SimpleInputSrcDetector<'a> {
    fn get_client_srcs(&self) -> Vec<&'a str> {
        self.srcs.clone()
    }

    fn get_input_srcs(&self) -> Vec<&'a str> {
        self.srcs.clone()
    }

    fn map_client_src<'b>(&self, src: &'b str) -> Option<&'b str> {
        if self.srcs.contains(&src) {
            Some(src)
        } else {
            None
        }
    }

    fn map_input_src<'b>(&self, src: &'b str) -> Option<&'b str> {
        if src == "time" {
            return Some("_time");
        }
        self.map_client_src(src)
    }
}
