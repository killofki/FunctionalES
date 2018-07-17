// to arrow with killofki version .. 
! function () { // var closer from outer this 
/// 

const curry2 = f => ( ... _ ) => 
	  _ .length < 2 ? ( ... _2 ) => f( ... _ , ... _2 ) 
	: f( ... _ ) 
	; 

const flip = f => ( ... _ ) => f( ... _ .reverse() ); 

const then = curry2( ( f, a ) => 
	  a instanceof Promise ? a .then( f ) 
	: f( a ) 
	); 

const { log } = console; 

const identity = a => a; 

const noop = () => {} 

const ObjIter = curry2( ( generator, coll, iter = generator( coll ) ) => 
	({ next : _ => iter .next(), [ Symbol .iterator ]() { return this } }) 
	) 
	; 

function * valuesIterator( coll ) { 
	for ( const key in coll ) { 
		yield coll[ key ]; 
		} 
	} 

function * entriesIterator( coll ) { 
	for ( const key in coll ) { 
		yield [ key, coll[ key ] ]; 
		} 
	} 

[ ObjIter, { values : valuesIterator, entries : entriesIterator } ] 
.reduce( ( F, o ) => {  
	for ( const [ p, v ] of entriesIterator( o ) ) { 
		o[ p ] = F( v ); 
		} 
	Object .assign( F, o ); 
	} ) 
	; 

const 
	  hasIter = a => !! ( a && a[ Symbol .iterator ] ) 
	, isObject = a => !! a && [ 'object', 'function' ] .some( t => 
		typeof a == t 
		) 
	; 

const valuesIter = coll => 
	  hasIter( coll ) ? coll[ 
		  typeof coll .values == 'function' ? 'values' 
		: Symbol .iterator 
		]() 
	: ObjIter .values( coll ) 
	; // isPlainObject 

const reduce = curry2( ( f, acc, coll ) => { 
	const iter = valuesIter( coll === undefined ? acc : coll ); 
	return then( 
		  function recur( acc ) { 
			for ( const val of iter ) { 
				if ( ( acc = f( acc, val ) ) instanceof Promise ) { 
					return acc .then( recur ); 
					} 
				} 
			return acc; 
			} // -- function recur() 
		, coll === undefined ? iter .next() .value : acc 
		) // -- return then() 
		; 
	} ) // -- reduce 
	; 

class Tuple { // callRight joiner 
	constructor( ... ar ) { this .value = ar; } 
	[ Symbol .iterator ]() { return this .value[ Symbol .iterator ](); } 
	} 

const tuple = ( ... args ) => 
	  args .length == 1 ? args[ 0 ] 
	: find( arg => arg instanceof Promise, args ) 
		? then( toTuple, Promise .all( args ) ) // toTuple from await Promise() 
	: new Tuple( ... args ) // construct Tuple 
	; 

const toTuple = list => 
	  list .length == 1 ? list[ 0 ] 
	: tuple( ... list ) 
	; // value or tuple list 

const callRight = ( arg, f ) => 
	  arg instanceof Tuple ? f( ... arg ) // join from Tuple 
	: arg === undefined ? f() 
	: f( arg ) 
	; 

const mfReduce = ( f1, f2, f3 ) => ( f, coll ) => 
	  coll instanceof Map ? reduce( f1( f ), new Map, coll .entries() ) 
	: hasIter( coll ) ? reduce( f2( f ), [], coll ) 
	: isObject( coll ) ? reduce( f3( f ), {}, ObjIter .entries( coll ) ) 
	: [] 
	; 

const _map = mfReduce( 
	  f => ( m, [ k, v ] ) => go( f( v ), v => 
		m .set( k, v ) 
		) 
	, f => ( arr, v ) => go( f( v ), v => ( 
		  arr .push( v ) 
		, arr 
		) ) 
	, f => ( o, [ k, v ] ) => go( f( v ), v => ( 
		  o[ k ] = v 
		, o 
		) ) 
	) 
	; 

const map = curry2( ( f, coll ) => 
	  coll instanceof Function ? pipe( coll, f ) 
	: coll instanceof Promise ? coll .then( f ) 
	: _map( f, coll ) 
	) 
	; 

const mapS = curry2( ( f, coll ) => reduce( 
	  ( a, b ) => `${ a }${ f( b ) }` 
	, '' 
	, coll 
	) ) 
	; 

const filter = curry2( mfReduce( 
	  f => ( m, [ k, v ] ) => go( f( v ), b => 
		  b ? m .set( k, v ) 
		: m 
		) 
	, f => ( arr, v ) => go( f( v ), b => ( 
		  b && arr .push( v ) 
		, arr 
		) ) 
	, f => ( o, [ k, v ] ) => go( f( v ), b => ( 
		  b && ( o[ k ] = v ) 
		, o 
		) ) 
	) ) 
	; 

const reject = curry2( ( f, coll ) => filter( negate( f ), coll ) ); 

const compact = filter( identity ); 

const 
	  negate = f => pipe( f, not ) 
	, complement = negate 
	; 

const not = a => ! a; 

const go = ( ... _ ) => reduce( callRight, _ ); 

const pipe = ( ... _fs ) => 
	Object .assign( 
		  ( ... _ ) => reduce( callRight, toTuple( _ ), _fs ) 
		, { _fs } 
		, hurdles 
		) 
	; 

const hurdles = map( 
	  name => function( ... fs2 ) { return hurdle( ... this ._fs )[ name ]( ... fs2 ) } 
	, { 
		  nullable : 'nullable' 
		, error : 'error' 
		, exception : 'exception' 
		, complete : 'complete' 
		} 
	) 
	; 

const tap = ( ... fs ) => { 
	const f = pipe( ... fs ); 
	return ( ... args ) => go( f( ... args ), _ => toTuple( args ) ); 
	} 
	; 

const hi = tap( log ); 

const each = curry2( ( f, coll ) => reduce( ( _, val ) => f( val ), null, coll ) ); 

const findVal = curry2( ( f, coll ) => { 
	const iter = valuesIter( coll ); 
	return function recur( res ) { 
		for ( const val of iter ) 
			if ( ( res = f( val ) ) !== undefined ) { 
				return then( res => res === undefined ? recur() : res, res ); 
				} 
		}() 
		; 
	} ) 
	; 

const find = curry2( ( f, coll ) => findVal( a => go( a, f, b => b ? a : undefined ), coll ) ); 

const isAny = a => a !== undefined; 
const some = curry2( pipe( find, isAny ) ); 

const isUndefined = a => a === undefined; 
const none = curry2( pipe( find, isUndefined ) ); 

const every = curry2( ( f, coll ) => { 
	var nf = negate( f ), hasLength = false; 
	return go( 
		  coll 
		, find( v => ( hasLength = true, nf( v ) ) ) 
		, v => hasLength && v === undefined 
		) 
		; 
	} ) 
	; 

const isMatch = curry2( ( a, b ) => 
	  typeof a == 'function' 
		? a( b ) 
	: Array .isArray( b ) && Array .isArray( a ) 
		? every( v => b .includes( v ), a ) 
	: typeof b == 'object' 
		? every( ([ k, v ]) => b[ k ] == v, ObjIter .entries( a ) ) 
	: a == b 
	) 
	; 

const findWhere = curry2( ( w, coll ) => find( isMatch( w ), coll ) ); 

const baseMatch = ( targets ) => { 
	var cbs = []; 
	
	function _evl() { 
		return go( 
			  cbs 
			, find( pb => pb ._case( ... targets ) ) 
			, pb => pb ._body( ... targets ) 
			) 
			; 
		} 
	
	function _case( f ) { 
		cbs .push({ 
			_case : 
				  typeof f == 'function' ? pipe( ... arguments ) 
				: isMatch( f ) 
			}); 
		return _body; 
		} 
	_case .case = _case; 
	
	function _body( ... ar ) { 
		cbs[ cbs .length - 1 ] ._body = pipe( ... ar ); 
		return _case; 
		} 
	
	_case .else = ( ... ar ) => (  
		  _case( _ => true )( ... ar ) 
		, targets ? _evl() 
		: ( ... targets2 ) => ( ( targets = targets2 ), _evl() ) 
		) 
		; 
	
	return _case; 
	} // -- baseMatch() 
	; 

const match = ( ... _ ) => baseMatch( _ ); 
match .case = ( ... _ ) => baseMatch( null ) .case( ... _ ); 

const or = ( ... fs ) => { 
	const last = fs .pop(); 
	return ( ... ar ) => go( 
		  fs 
		, findVal( pipe( 
			  f => f( ... ar ) 
			, a => a ? a : undefined 
			) ) 
		, a => a ? a : last( ... ar ) 
		) 
		; 
	} // -- or 
	; 

const and = ( ... fs ) => { 
	const last = fs .pop(); 
	return ( ... ar ) => go( 
		  fs 
		, findVal( pipe( 
			  f => f( ... ar ) 
			, a => a ? undefined : a 
			) ) 
		, a => a === undefined ? last( ... ar ) : a 
		) 
		; 
	} 
	; 

function stepIter( data, limit ) { 
	var iter = valuesIter( data ), i = 0; 
	return limit == Infinity ? iter 
		: { 
			  [ 'next' ]() { // next : 
				if ( i++ == limit ) { 
					i = 0; 
					return { value: undefined, done: true }; 
					} 
				var cur = iter .next(); 
				this .remain = ! cur .done; 
				return cur; 
				} 
			, [ Symbol .iterator ]() { return this; } 
			, remain : true 
			} 
		; 
	} 

const mapIter = curry2( ( f, iter, res = [] ) => { 
	for ( const val of iter ) res .push( f( val ) ); 
	return res; 
	} ) 
	; 

const pAall = l => Promise .all( l ); 

function mapCReduce( acc, iter, mapF, extendF ) { 
	return go( 
		  mapIter( mapF, iter ) 
		, pAall 
		, l => extendF( acc, l ) 
		, _ => iter .remain ? mapCReduce( acc, iter, mapF, extendF ) : acc 
		) 
		; 
	} 

const setPair = f => pair => go( pair[ 1 ], f, v => ( 
	  pair[ 1 ] = v 
	, pair 
	) ); 

const mapC = curry2( ( f, coll, limit = Infinity ) => 
	  coll instanceof Map ? mapCReduce( 
		  new Map, stepIter( coll .entries(), limit ) 
		, setPair( f ) 
		, ( acc, l ) => l .forEach( ([ k, v ]) => acc .set( k, v ) ) 
		) 
	: hasIter( coll ) ? 
		  limit == Infinity ? pAall( mapIter( f, coll ) ) 
		: mapCReduce( [], stepIter( coll, limit ), f, ( acc, l ) => acc .push( ... l ) ) 
	: isObject( coll ) ? mapCReduce( 
		  {} 
		, stepIter( ObjIter .entries( coll ), limit ) 
		, setPair( f ) 
		, ( acc, l ) => l .forEach( ([ k, v ]) => 
			acc[ k ] = v 
			) 
		) 
	: [] 
	) 
	; 

const series = ( ... _ ) => map( a => a(), ... _ ); 
const concurrency = ( ... _ ) => mapC( a => a(), ... _ ); 

const thenCatch = ( f, catchF, a ) => { 
	try { 
		return a instanceof Promise ? a .then( f, catchF ) 
			: f( a ) 
			; 
		} 
	catch ( e ) { return catchF( e ); } 
	} 
	; 

const findValC = curry2( ( f, coll, limit = Infinity ) => { 
	const iter = stepIter( coll, limit ); 
	return new Promise( findValCThread ); 
	
	function findValCThread( resolve, reject ) { 
		! function recur() { 
			var t = 0, r = 0; 
			for ( const a of iter ) { 
				++ t; 
				thenCatch( 
					  b => 
						  b === undefined 
							? t == ++r && iter .remain && recur() 
						: resolve( b ) 
					, reject 
					, f( a ) 
					); 
				} 
			}() 
			; 
		} 
	} ) 
	; 

const findC = curry2( ( f, coll, limit ) => findValC( 
	  a => go( a, f, b => b ? a : undefined ) 
	, coll 
	, limit 
	) ); 
const someC = curry2( pipe( findC, isAny ) ); 
const noneC = curry2( pipe( findC, isUndefined ) ); 
const everyC = curry2( ( f, coll ) => { 
	var nf = negate( f ), hasLength = false; 
	return go( 
		  coll 
		, findC( v => ( hasLength = true, nf( v ) ) ) 
		, v => hasLength && v === undefined 
		) 
		; 
	} ) 
	; 
const findWhereC = curry2( ( w, coll ) => findC( isMatch( w ), coll ) ); 

function hurdle( ... fs ) { 
	var errorF, nullableF, completeF, exceptions = []; 
	
	function evaluator( ... ar ) { 
		var error = false, catched = false; 
		if ( ! errorF && ! exceptions .length && ! nullableF ) { 
			nullableF = noop; 
			} 
		return go( 
			  reduce( 
				  ( arg, f ) => 
					  errorF && error ? error 
					: nullableF && arg == null ? arg 
					: catched ? arg 
					: go( 
						  find( pnb => callRight( arg, pnb .predi ), exceptions ) 
						, pnb => ( 
							  pnb ? ( 
								  catched = true 
								, callRight( arg, pnb .body ) 
								) 
							: ! errorF ? callRight( arg, f ) 
							: ( q => { 
								try { 
									var res = callRight( arg, f ); 
									res = res instanceof Promise 
											? res .then( identity, err => error = err ) 
										: res 
										; 
									} 
								catch ( err ) { error = err; } 
								return res; 
								} )() 
							) 
						) 
				, toTuple( ar ) 
				, fs 
				) 
			, res => 
				  catched ? res 
				: error ? 
					  errorF ? errorF( error ) 
					: log( 'Uncaught Error: ', error ) 
				: nullableF && res == null ? nullableF( res ) 
				: completeF ? completeF( res ) 
				: res 
			) 
			; 
		} // -- evaluator() 
	
	Object .assign( evaluator, { 
		  nullable : ( ... fs ) => ( 
			  ( nullableF = fs .length ? pipe( ... fs ) : identity )  
			, evaluator 
			) 
		, error : ( ... fs ) => ( 
			  ( errorF = pipe( ... fs ) ) 
			, evaluator 
			) 
		, exception : ( ... fs ) => { 
			var pnb = { predi : pipe( ... fs ) }; 
			return ( ... fs ) => ( 
				  ( pnb .body = pipe( ... fs ) ) 
				, exceptions .push( pnb ) 
				, evaluator 
				) 
				; 
			} 
		, complete : ( ... fs ) => ( 
			  ( completeF = pipe( ... fs ) ) 
			, evaluator 
			) 
		} ) 
		; 
	
	return evaluator; 
	} // -- hurdle() 

const baseSel = sep => curry2( ( selector, acc ) => 
	  Array .isArray( selector ) 
		? reduce( flip( baseSel( sep ) ), acc, selector ) 
	: isObject( selector ) 
		? findWhere( selector, acc ) 
	: reduce( ( acc, key, tk = key .trim(), s = tk[ 0 ] ) => 
		  ! acc ? acc 
		: s == '#' 
			? findWhere( { id : tk.substr( 1 ) }, acc ) 
		: s == '[' || s == '{' 
			? findWhere( JSON .parse( tk ), acc ) 
		: acc[ tk ] 
		, acc 
		, selector .split( sep ) 
		) 
	) 
	; 

const sel = baseSel( ' > ' ); 

const Functional = { 
	  curry2, flip 
	, then, identity, noop 
	, ObjIter, valuesIter, stepIter, hasIter, isObject 
	, map, mapC, mapS, series, concurrency 
	, filter, reject, compact 
	, reduce 
	, go, pipe, tap, hi 
	, findVal, find, some, none, every, findWhere 
	, findValC, findC, someC, noneC, everyC, findWhereC 
	, baseSel, sel 
	, match, or, and, isMatch 
	, Tuple, tuple, toTuple, callRight 
	, negate, complement, not, isAny, isUndefined 
	, each, log 
	} 
	; 

if ( typeof global == 'object' ) { 
	module .exports = Functional; 
	} 
else { 
	window .Functional = Functional; 
	} 

/// 
	}() 
&& console .log( 'Functional Loaded.' ) 
	; 

// original https://github.com/Functional-JavaScript/FunctionalES/blob/master/functional.es.js 
// twit from https://twitter.com/adhrinae/status/1009424934693568512 