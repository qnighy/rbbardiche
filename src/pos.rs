use bstr::ByteSlice;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

const SPARSE_POSITION_INTERVAL: usize = 64;

#[derive(Debug, Clone)]
pub struct SourceLocator {
    line_endings: Vec<usize>,
    sparse_line_index: Vec<u32>,
}

impl SourceLocator {
    pub fn new(source: &[u8]) -> Self {
        let mut line_endings = Vec::new();
        let mut i = 0;
        while i < source.len() {
            let ch = source[i];
            if ch == b'\n' || (ch == b'\r' && (i + 1 >= source.len() || source[i + 1] != b'\n')) {
                line_endings.push(i + 1);
            }
            i += 1;
        }

        let mut sparse_line_index = vec![0; source.len() / SPARSE_POSITION_INTERVAL + 1];
        let mut line = 0;
        for i in 0..sparse_line_index.len() {
            while line < line_endings.len()
                && (line_endings[line] as usize) <= i * SPARSE_POSITION_INTERVAL
            {
                line += 1;
            }
            sparse_line_index[i] = line as u32;
        }
        SourceLocator {
            line_endings,
            sparse_line_index,
        }
    }

    pub fn position_general<F>(&self, source: &[u8], index: usize, counter: F) -> Position
    where
        F: FnOnce(&[u8]) -> u32,
    {
        let mut line = self.sparse_line_index[index / SPARSE_POSITION_INTERVAL];
        let mut i = index / SPARSE_POSITION_INTERVAL * SPARSE_POSITION_INTERVAL;
        while i < index {
            let ch = source[i];
            if ch == b'\n' || (ch == b'\r' && (i + 1 >= source.len() || source[i + 1] != b'\n')) {
                line += 1;
            }
            i += 1;
        }
        let line_beginning = if line == 0 {
            0
        } else {
            self.line_endings[line as usize - 1]
        };
        let character = counter(&source[line_beginning..index]);
        Position { line, character }
    }

    pub fn position_utf8(&self, source: &[u8], index: usize) -> Position {
        self.position_general(source, index, |line| line.len() as u32)
    }

    pub fn position_utf16(&self, source: &[u8], index: usize) -> Position {
        self.position_general(source, index, |line| {
            line.chars()
                .map(|ch| if (ch as u32) < 0x10000 { 1 } else { 2 })
                .sum()
        })
    }
}
