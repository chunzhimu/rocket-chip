// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.math.max

/** Options for memory regions */
object RegionType {
  sealed trait T
  case object CACHED      extends T
  case object TRACKED     extends T
  case object UNCACHED    extends T
  case object UNCACHEABLE extends T
  val cases = Seq(CACHED, TRACKED, UNCACHED, UNCACHEABLE)
}

// A non-empty half-open range; [start, end)
case class IdRange(start: Int, end: Int)
{
  require (start >= 0)
  require (end >= 0)
  require (start < end) // not empty

  // This is a strict partial ordering
  def <(x: IdRange) = end <= x.start
  def >(x: IdRange) = x < this

  def overlaps(x: IdRange) = start < x.end && x.start < end
  def contains(x: IdRange) = start <= x.start && x.end <= end
  // contains => overlaps (because empty is forbidden)

  def contains(x: Int)  = start <= x && x < end
  def contains(x: UInt) = UInt(start) <= x && x < UInt(end) // !!! special-case =

  def shift(x: Int) = IdRange(start+x, end+x)
}

// An potentially empty inclusive range of 2-powers [min, max] (in bytes)
case class TransferSizes(min: Int, max: Int)
{
  def this(x: Int) = this(x, x)

  require (min <= max)
  require (min != 0 || max == 0)
  require (max == 0 || isPow2(max))
  require (min == 0 || isPow2(min))

  def none = min == 0
  def contains(x: Int) = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int) = contains(1 << x)
  def containsLg(x: UInt) = if (none) Bool(false) else { UInt(log2Ceil(min)) <= x && x <= UInt(log2Ceil(max)) } // !!! special-case =

  def contains(x: TransferSizes) = x.none || (min <= x.min && x.max <= max)
  
  def intersect(x: TransferSizes) =
    if (x.max < min || min < x.max) TransferSizes.none
    else TransferSizes(scala.math.max(min, x.min), scala.math.min(max, x.max))
}

object TransferSizes {
  def apply(x: Int) = new TransferSizes(x)
  val none = new TransferSizes(0)

  implicit def asBool(x: TransferSizes) = !x.none
}

// AddressSets specify the mask of bits consumed by the manager
// The base address used by the crossbar for routing
case class AddressSet(mask: BigInt, base: Option[BigInt] = None)
{
  // Forbid empty sets
  require (base == None || (base.get & mask) == 0)

  def contains(x: BigInt) = ((x ^ base.get) & ~mask) == 0
  def contains(x: UInt) = ((x ^ UInt(base.get)) & UInt(~mask)) === UInt(0)

  // overlap iff bitwise: both care (~mask0 & ~mask1) => both equal (base0=base1)
  // if base = None, it will be auto-assigned and thus not overlap anything
  def overlaps(x: AddressSet) = (base, x.base) match {
    case (Some(tbase), Some(xbase)) => (~(mask | x.mask) & (tbase ^ xbase)) == 0
    case _ => false
  }

  // contains iff bitwise: x.mask => mask && contains(x.base)
  def contains(x: AddressSet) = ((x.mask | (base.get ^ x.base.get)) & ~mask) == 0
  // 1 less than the number of bytes to which the manager should be aligned
  def alignment1 = ((mask + 1) & ~mask) - 1
  def max = base.get | mask
}

case class TLManagerParameters(
  address:            Seq[AddressSet],
  sinkId:             IdRange       = IdRange(0, 1),
  regionType:         RegionType.T  = RegionType.UNCACHEABLE,
  // Supports both Acquire+Release+Finish of these sizes
  supportsAcquire:    TransferSizes = TransferSizes.none,
  supportsArithmetic: TransferSizes = TransferSizes.none,
  supportsLogical:    TransferSizes = TransferSizes.none,
  supportsGet:        TransferSizes = TransferSizes.none,
  supportsPutFull:    TransferSizes = TransferSizes.none,
  supportsPutPartial: TransferSizes = TransferSizes.none,
  supportsHint:       Boolean       = false,
  // If fifoId=Some, all messages sent to the same fifoId are delivered in FIFO order
  fifoId:             Option[Int]   = None)
{
  address.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.overlaps(y))
  })
  address.foreach({ case a =>
    require (supportsAcquire.none || a.alignment1 >= supportsAcquire.max-1)
  })

  // Largest support transfer of all types
  val maxTransfer = List(
    supportsAcquire.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
}

case class TLManagerPortParameters(managers: Seq[TLManagerParameters], beatBytes: Int)
{
  require (isPow2(beatBytes))

  // Require disjoint ranges for Ids and addresses
  managers.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sinkId.overlaps(y.sinkId))
    x.address.foreach({ a => y.address.foreach({ b =>
      require (!a.overlaps(b))
    })})
  })

  // Bounds on required sizes
  def endSinkId   = managers.map(_.sinkId.end).max
  def maxAddress  = managers.map(_.address.map(_.max).max).max
  def maxTransfer = managers.map(_.maxTransfer).max
  
  // Operation sizes supported by all outward Managers
  val allSupportAcquire    = managers.map(_.supportsAcquire)   .reduce(_ intersect _)
  val allSupportArithmetic = managers.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = managers.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = managers.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = managers.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = managers.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = managers.map(_.supportsHint)      .reduce(_    &&     _)

  // These return Option[TLManagerParameters] for your convenience
  def find(address: BigInt) = managers.find(_.address.exists(_.contains(address)))
  def findById(id: Int) = managers.find(_.sinkId.contains(id))

  // Synthesizable lookup methods
  def find(address: UInt) = Vec(managers.map(_.address.map(_.contains(address)).reduce(_ || _)))
  def findById(id: UInt) = Vec(managers.map(_.sinkId.contains(id)))
  
  // Does this Port manage this ID/address?
  def contains(address: UInt) = find(address).reduce(_ || _)
  def containsById(id: UInt) = findById(id).reduce(_ || _)
  
  private def safety_helper(member: TLManagerParameters => TransferSizes)(address: UInt, lgSize: UInt) = {
    (find(address) zip managers.map(member(_).containsLg(lgSize)))
    .map { case (m, s) => m && s } reduce (_ || _)
  }

  // Check for support of a given operation at a specific address
  val supportsAcquire    = safety_helper(_.supportsAcquire)    _
  val supportsArithmetic = safety_helper(_.supportsArithmetic) _
  val supportsLogical    = safety_helper(_.supportsLogical)    _
  val supportsGet        = safety_helper(_.supportsGet)        _
  val supportsPutFull    = safety_helper(_.supportsPutFull)    _
  val supportsPutPartial = safety_helper(_.supportsPutPartial) _
  def supportsHint(address: UInt) = {
    (find(address) zip managers.map(_.supportsHint))
    .map { case (m, b) => m && Bool(b) } reduce (_ || _)
  }
}

case class TLClientParameters(
  sourceId:            IdRange       = IdRange(0,1),
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes = TransferSizes.none,
  supportsArithmetic:  TransferSizes = TransferSizes.none,
  supportsLogical:     TransferSizes = TransferSizes.none,
  supportsGet:         TransferSizes = TransferSizes.none,
  supportsPutFull:     TransferSizes = TransferSizes.none,
  supportsPutPartial:  TransferSizes = TransferSizes.none,
  supportsHint:        Boolean       = false)
{
  val maxTransfer = List(
    supportsProbe.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
}

case class TLClientPortParameters(clients: Seq[TLClientParameters]) {
  // Require disjoint ranges for Ids
  clients.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sourceId.overlaps(y.sourceId))
  })

  // Bounds on required sizes
  def endSourceId = clients.map(_.sourceId.end).max
  def maxTransfer = clients.map(_.maxTransfer).max

  // Operation sizes supported by all inward Clients
  val allSupportProbe      = clients.map(_.supportsProbe)     .reduce(_ intersect _)
  val allSupportArithmetic = clients.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = clients.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = clients.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = clients.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = clients.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = clients.map(_.supportsHint)      .reduce(_    &&     _)

  // These return Option[TLClientParameters] for your convenience
  def find(id: Int) = clients.find(_.sourceId.contains(id))
  
  // Synthesizable lookup methods
  def find(id: UInt) = Vec(clients.map(_.sourceId.contains(id)))
  def contains(id: UInt) = find(id).reduce(_ || _)
  
  private def safety_helper(member: TLClientParameters => TransferSizes)(address: UInt, lgSize: UInt) = {
    (find(address) zip clients.map(member(_).containsLg(lgSize)))
    .map { case (m, s) => m && s } reduce (_ || _)
  }

  // Check for support of a given operation at a specific id
  val supportsProbe      = safety_helper(_.supportsProbe)      _
  val supportsArithmetic = safety_helper(_.supportsArithmetic) _
  val supportsLogical    = safety_helper(_.supportsLogical)    _
  val supportsGet        = safety_helper(_.supportsGet)        _
  val supportsPutFull    = safety_helper(_.supportsPutFull)    _
  val supportsPutPartial = safety_helper(_.supportsPutPartial) _
  def supportsHint(address: UInt) = {
    (find(address) zip clients.map(_.supportsHint))
    .map { case (m, b) => m && Bool(b) } reduce (_ || _)
  }
}

case class TLBundleParameters(
  addressBits: Int,
  dataBits:    Int,
  sourceBits:  Int,
  sinkBits:    Int,
  sizeBits:    Int)
{
  // Chisel has issues with 0-width wires
  require (addressBits >= 1)
  require (dataBits    >= 1)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))
  
  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addressBits, x.addressBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits))
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Up(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes)

  val bundle = TLBundleParameters(
    addressBits = log2Up(manager.maxAddress + 1),
    dataBits    = manager.beatBytes * 8,
    sourceBits  = log2Up(client.endSourceId),
    sinkBits    = log2Up(manager.endSinkId),
    sizeBits    = log2Up(maxLgSize+1))

  def isAligned(address: UInt, lgSize: UInt) =
    if (maxLgSize == 0) Bool(true) else {
      val mask = Vec.tabulate(maxLgSize) { UInt(_) < lgSize }
      address & mask.toBits.asUInt === UInt(0)
    }

  // This gets used everywhere, so make the smallest circuit possible ...
  def fullMask(address: UInt, lgSize: UInt) = {
    val lgBytes = log2Ceil(manager.beatBytes)
    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= UInt(lgBytes), Bool(true)))
      } else {
        val sub = helper(i-1)
        val size = lgSize === UInt(lgBytes - i)
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && address(lgBytes - i) === Bool(j % 2 == 1)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }
    Vec(helper(lgBytes).map(_._1)).toBits.asUInt
  }
}
