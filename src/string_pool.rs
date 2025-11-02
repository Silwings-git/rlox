use std::{
    collections::HashMap,
    fmt::Display,
    hash::{DefaultHasher, Hash, Hasher},
    ops::Add,
    rc::Rc,
};

#[derive(Default, Debug)]
pub struct StringPool {
    pool: HashMap<Rc<str>, InternedString>,
}

impl StringPool {
    pub fn intern(&mut self, s: &str) -> InternedString {
        if let Some(existing) = self.pool.get(s) {
            return existing.clone();
        }

        let rc: Rc<str> = Rc::from(s);
        let interned = InternedString::new(rc.clone());

        self.pool.insert(rc, interned.clone());

        interned
    }

    pub fn intern_interned(&mut self, s: &InternedString) -> InternedString {
        if let Some(existing) = self.pool.get(&s.str) {
            existing.clone()
        } else {
            // 如果 interner 中暂时没有，直接复用 Rc 和 hash（不重新 Rc::from）
            let cloned = s.clone();
            self.pool.insert(cloned.str.clone(), cloned.clone());
            cloned
        }
    }
}

#[derive(Debug, Clone)]
pub struct InternedString {
    str: Rc<str>,
    hash: u64,
}

impl AsRef<InternedString> for InternedString {
    fn as_ref(&self) -> &InternedString {
        self
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.str, &other.str) || self.str == other.str
    }
}

impl Eq for InternedString {}

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{:p}:{}", self.str, self.str)
        write!(f, "{}", self.str)
    }
}

impl Add for &InternedString {
    type Output = InternedString;

    fn add(self, rhs: Self) -> Self::Output {
        let combined = format!("{}{}", self.as_str(), rhs.as_str());
        InternedString::new(combined.into())
    }
}

impl Add for InternedString {
    type Output = InternedString;

    fn add(self, rhs: Self) -> Self::Output {
        let combined = format!("{}{}", self.as_str(), rhs.as_str());
        InternedString::new(combined.into())
    }
}

impl InternedString {
    pub fn new(str: Rc<str>) -> Self {
        let hash = {
            let mut hasher = DefaultHasher::new();
            str.hash(&mut hasher);
            hasher.finish()
        };
        Self { str, hash }
    }

    /// 暴露内部字符串的不可变引用
    pub fn as_str(&self) -> &str {
        &self.str
    }
}
