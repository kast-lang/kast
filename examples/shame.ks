use std;

const Granularity = newtype .PerThread | .PerDraw;

const min = fn (~A :: Granularity, ~B :: Granularity) -> Granularity {
	.PerDraw
};

const float = forall[G :: Granularity] {
	(value: float64) as type
};

const @"op binary +" = forall [~LHS :: Granularity, ~RHS :: Granularity] {
	fn (~lhs :: float[LHS], ~rhs :: float[RHS]) -> float[min(A: LHS, B: RHS)] {
		value: 123
	}
};

const @"op binary *" = forall [~LHS :: Granularity, ~RHS :: Granularity] {
	fn (~lhs :: float[LHS], ~rhs :: float[RHS]) -> float[min(A: LHS, B: RHS)] {
		value: 123
	}
};

const fma = forall[XG :: Granularity, YG :: Granularity, ZG :: Granularity] {
	fn (x :: float[XG], y :: float[YG], z :: float[ZG]) -> float[min(XG, YG, ZG)] {
		(x * y) + z
	}
};

const x :: float[.PerThread] = _;
const y :: float[.PerDraw] = _;
const z :: float[.PerThread] = _;

dbg x;

dbg <| fma(~x, ~y, ~z);
