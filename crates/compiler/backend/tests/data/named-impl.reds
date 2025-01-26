module CurveData

@nameImplementation(CurveDataFloat as CurveData<Float>)
native struct CurveData<A> {
    native static func Create() -> CurveData<A>
}

func Test() {
  let _: CurveData<Float> = CurveData.Create();
}
