/// 虚拟机的核心配置项
#[derive(Debug, Clone, Copy)]
pub struct VMConfig {
    // 调用帧的最大深度
    pub max_frames_depth: usize,
    /// 栈的最大深度限制
    pub max_stack_depth: usize,
}

impl Default for VMConfig {
    fn default() -> Self {
        let max_frames_depth = 64;
        Self {
            max_frames_depth,
            max_stack_depth: max_frames_depth * u8::MAX as usize,
        }
    }
}

impl VMConfig {
    /// 创建自定义配置的便捷方法
    pub fn new() -> Self {
        Self::default()
    }

    /// 设置栈最大深度
    #[allow(dead_code)]
    pub fn with_max_stack_depth(mut self, depth: usize) -> Self {
        self.max_stack_depth = depth;
        self
    }
}
