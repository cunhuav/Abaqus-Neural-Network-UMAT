      module simple_module
      implicit none
      private
      public :: CreateNetwork, LoadNetworkParams, EvaluateNetwork, NormalizeInput, DenormalizeOutput

      !double precision, allocatable :: MinInput(:), MaxInput(:), MinOutput(:), MaxOutput(:)
    
      type Layer
         double precision, allocatable, dimension(:,:) :: Weights
         double precision, allocatable, dimension(:) :: Biases
      end type Layer
    
      type Network
         type(layer),allocatable, dimension(:) :: Layers
         integer :: NumLayers
         double precision, allocatable :: MinInput(:), MaxInput(:), MinOutput(:), MaxOutput(:)
      end type Network
    
      type(Network),allocatable, dimension(:) :: Networks
      integer :: NumNetworks = 0        
     
      contains
    
      !create a new network
      subroutine CreateNetwork(network_id)
      implicit none
      integer, intent(out) :: network_id
      type(Network), allocatable, dimension(:) :: temp
      integer :: i

    
      !dynamic extension
      NumNetworks = NumNetworks + 1
      if (.not.allocated(Networks)) then
          allocate(Networks(NumNetworks))
      else
          allocate(temp(size(Networks)))
          temp = Networks
          deallocate(Networks)
          allocate(Networks(NumNetworks))
          Networks(1:min(size(temp), NumNetworks)) = temp(1:min(size(temp), NumNetworks))
          deallocate(temp)
      end if

      ! 找到未使用的最小网络 ID
      do i = 1, NumNetworks
          if (.not.allocated(Networks(i)%Layers)) then
              network_id = i
              print *, 'Not Allocated Network ID:', network_id
              return
          end if
      end do
     
      network_id = NumNetworks
      print *, 'NumNetworks:', NumNetworks
      print *, 'Now the Network ID:', network_id    
      end subroutine CreateNetwork
    
      !load network parameters
      subroutine LoadNetworkParams(network_id,filename)
      implicit none
      integer, intent(in) :: network_id
      character(len=*) :: filename
      integer :: i, j, ios, rows, cols, bias_size, input_size, output_size
    
      if (network_id > NumNetworks) then
          print *, 'Error: Network ID does not exist'
          return
      end if
    
      !open file
      open(10, file=filename, status='old',action='read',iostat=ios)
      if(ios /= 0) then
          print *, 'Error: Unable to open file',filename
          return
      end if
    
      read(10,*) input_size, output_size
      print *, "Input size:", input_size, "Output size:", output_size
      IF (ALLOCATED(Networks(network_id)%MinInput)) DEALLOCATE(Networks(network_id)%MinInput)
      ALLOCATE(Networks(network_id)%MinInput(input_size))
    
      IF (ALLOCATED(Networks(network_id)%MaxInput)) DEALLOCATE(Networks(network_id)%MaxInput)
      ALLOCATE(Networks(network_id)%MaxInput(input_size))

      IF (ALLOCATED(Networks(network_id)%MinOutput)) DEALLOCATE(Networks(network_id)%MinOutput)
      ALLOCATE(Networks(network_id)%MinOutput(output_size))

      IF (ALLOCATED(Networks(network_id)%MaxOutput)) DEALLOCATE(Networks(network_id)%MaxOutput)
      ALLOCATE(Networks(network_id)%MaxOutput(output_size))

      !read normalization parameters
      READ(10,*) Networks(network_id)%MinInput
      READ(10,*) Networks(network_id)%MaxInput
      READ(10,*) Networks(network_id)%MinOutput
      READ(10,*) Networks(network_id)%MaxOutput
      PRINT *, "Normalization parameters loaded."
    !   PRINT *, "MinInput:", Networks(network_id)%MinInput
    !   PRINT *, "MaxInput:", Networks(network_id)%MaxInput
    !   PRINT *, "MinOutput:", Networks(network_id)%MinOutput
    !   PRINT *, "MaxOutput:", Networks(network_id)%MaxOutput
    
      !read number of layers
      read(10,*) Networks(network_id)%NumLayers
      allocate(Networks(network_id)%Layers(Networks(network_id)%NumLayers))
    !   print *, "Number of layers:", Networks(network_id)%NumLayers
    
      !read layer parameters
      do i=1,Networks(network_id)%NumLayers
          read(10,*) rows, cols
          allocate(Networks(network_id)%Layers(i)%Weights(rows,cols))
        !   print *, "Rows:", rows, "Cols:", cols

          !weights
          do j=1,rows
              read(10,*) Networks(network_id)%Layers(i)%Weights(j,:)
          end do
        !   print *, "Weights for layer", i, ":"
        !   do j = 1, rows
        !       write(*, '(100(F10.5, 1X))') Networks(network_id)%Layers(i)%Weights(j, :)
        !   end do

          !biases
          read(10,*) bias_size
        !   print *, "Bias size:", bias_size
          allocate(Networks(network_id)%Layers(i)%Biases(bias_size))
          read(10,*) Networks(network_id)%Layers(i)%Biases
        !   print *, Networks(network_id)%Layers(i)%Biases
      end do
      close(10)
      end subroutine LoadNetworkParams
    
      ! normalize input
      function NormalizeInput(network_id,input) result(normalized_input)
      implicit none
      integer, intent(in) :: network_id
      double precision, intent(in),dimension(:) :: input
      double precision, allocatable,dimension(:) :: normalized_input
      integer :: i
    
      if (.not.allocated(normalized_input)) then
          allocate(normalized_input(size(input)))
      end if

      do i = 1,size(input)
          normalized_input(i) = (input(i) - Networks(network_id)%MinInput(i)) / &
                                 (Networks(network_id)%MaxInput(i) - Networks(network_id)%MinInput(i))
      end do
      end function NormalizeInput
    
      !denormalize output
      function DenormalizeOutput(network_id,output) result(denormalized_output)
      implicit none
      integer, intent(in) :: network_id
      double precision, intent(in),dimension(:) :: output
      double precision, allocatable,dimension(:) :: denormalized_output
      integer :: i

      if (.not.allocated(denormalized_output)) then
          allocate(denormalized_output(size(output)))
      end if

      do i = 1,size(output)
          denormalized_output(i) = output(i) * (Networks(network_id)%MaxOutput(i) - Networks(network_id)%MinOutput(i)) + &
                                     Networks(network_id)%MinOutput(i)
      end do
      end function DenormalizeOutput
    
      !forward propagation
      function EvaluateNetwork(network_id, input) result(output)
      implicit none
      integer, intent(in) :: network_id
      double precision, intent(in),dimension(:) :: input
      double precision, allocatable,dimension(:) :: output
      double precision, allocatable,dimension(:) :: current_output
      integer :: i,j
      double precision:: sum
    
      if (network_id > NumNetworks) then
          print *, 'Error: Network ID does not exist'
          print *, 'Network ID:', network_id
          print *, 'NumNetworks:', NumNetworks
          return
      end if
    
      !initialize input
      current_output = NormalizeInput(network_id,input)
    
      do i = 1,Networks(network_id)%NumLayers
          !allocate output
          if (allocated(output)) then
              deallocate(output)
          end if
          allocate(output(size(Networks(network_id)%Layers(i)%Biases)))
        !   print *, 'input:'
        !   write(*, '(100(F10.5, 1X))') current_output
          do j = 1, size(Networks(network_id)%Layers(i)%Biases)
              sum = dot_product(Networks(network_id)%Layers(i)%Weights(:,j),current_output) + Networks(network_id)%Layers(i)%Biases(j)
              if (i<Networks(network_id)%NumLayers) then
                  output(j) = max(0.0d0,sum) ! ReLU activation function
              else
                  output(j) = sum ! linear activation function
              end if
          end do
        !   print *, 'output :'
        !   write(*, '(100(F10.5, 1X))') output
          !update current output
          if (allocated(current_output)) then
              deallocate(current_output)
          end if
          current_output = output
      end do
    
    
      !return denormalized output
      output = DenormalizeOutput(network_id,current_output)
      if (allocated(current_output)) then
          deallocate(current_output)
      end if
      end function EvaluateNetwork
    
      end module simple_module

      module NetworkManager
      use simple_module
      implicit none
      integer :: NET_ID = -1
      integer :: STIFF_ID = -1
      logical :: NETWORK_INITIALIZED = .FALSE.
      save NET_ID, STIFF_ID, NETWORK_INITIALIZED
      contains
      subroutine InitializeNetworks()
          if (.NOT. NETWORK_INITIALIZED) then
              call CreateNetwork(NET_ID)
              call LoadNetworkParams(NET_ID, "C:\\hello\\ro_umat\\RO_stress4.dat")
              call CreateNetwork(STIFF_ID)
              call LoadNetworkParams(STIFF_ID, "C:\\hello\\ro_umat\\RO_Ct4.dat")
              NETWORK_INITIALIZED = .TRUE.
              print *, "Networks successfully initialized. NET_ID:", NET_ID, "STIFF_ID:", STIFF_ID
          end if
      end subroutine InitializeNetworks
      end module NetworkManager      
!****************************************************************************************
!**********************Main Program**********************************************
!****************************************************************************************
!****************************************************************************
!  PROGRAM: test_nn_umat
!****************************************************************************

! program test_nn_umat
!     use simple_module
!     use NetworkManager

!     implicit none
!     double precision, dimension(6) :: input_data
!     double precision, allocatable, dimension(:) :: output_stress, output_stiffness

!     call InitializeNetworks()

!     ! Initialize test input
!     input_data = (/0.,         0.,         0.,         0.,         0.,         0.00342202/)

!     ! 计算 Stress
!     print *, "network id:", NET_ID
!     output_stress = EvaluateNetwork(NET_ID, input_data)
!     print *, "Stress = "
!     print '(F10.4)', output_stress

!     ! 计算 Stiffness
!     print *, "stiffness id:", STIFF_ID
!     output_stiffness = EvaluateNetwork(STIFF_ID, input_data)
!     print *, "Stiffness = "
!     print '(F20.4)', output_stiffness

! end program test_nn_umat

!****************************************************************************
!  PROGRAM: test_nn_umat with two neural networks
!****************************************************************************
! program test_nn_umat
!     use simple_module
!     use NetworkManager

!     implicit none

!     integer :: i, num_steps, io_status
!     double precision, allocatable, dimension(:,:) :: strain_history
!     double precision, dimension(6) :: input_data
!     double precision, allocatable, dimension(:) :: output_stress, output_stiffness
!     character(len=100) :: input_filename, output_filename
!     integer :: unit_input, unit_output

!     ! 文件名
!     input_filename = "strain_history.csv"
!     output_filename = "output_results.csv"

!     ! 1. 初始化神经网络
!     CALL InitializeNetworks()

!     ! 2. 打开 CSV 文件（输入）
!     open(unit=10, file=input_filename, status="old", action="read", iostat=io_status)
!     if (io_status /= 0) then
!         print *, "Error: Cannot open input file ", trim(input_filename)
!         stop
!     end if

!     ! 计算行数（num_steps）
!     num_steps = 0
!     do
!         read(10, *, iostat=io_status)
!         if (io_status /= 0) exit
!         num_steps = num_steps + 1
!     end do
!     rewind(10)  ! 重新回到文件开头

!     ! 3. 分配 strain_history 数组
!     allocate(strain_history(6, num_steps))
    
!     ! 读取 strain_history 数据
!     do i = 1, num_steps
!         read(10, *) strain_history(:, i)
!     end do
!     close(10)

!     ! 4. 打开 CSV 文件（输出）
!     open(unit=11, file=output_filename, status="replace", action="write", iostat=io_status)
!     if (io_status /= 0) then
!         print *, "Error: Cannot open output file ", trim(output_filename)
!         stop
!     end if

!     ! 写入 CSV 文件的标题
!     write(11, '(A)') "Step,Strain1,Strain2,Strain3,Strain4,Strain5,Strain6,Stress1,Stress2,Stress3,Stress4,Stress5,Stress6"

!     ! 5. 计算应力和刚度，并写入 CSV 文件
!     do i = 1, num_steps
!         input_data = strain_history(:, i)
!         print *, "Step ", i, ": Strain = ", input_data
        
!         ! 计算 Stress
!         output_stress = EvaluateNetwork(NET_ID, input_data)
!         print *, "Stress = ", output_stress

!         ! 输出结果到 CSV
!         write(11, '(I5,6F10.4,6F10.4,6F10.4)') i, input_data, output_stress
!     end do

!     ! 6. 关闭文件，释放内存
!     close(11)
!     deallocate(strain_history)

!     print *, "Results written to ", trim(output_filename)

! end program test_nn_umat

!****************************************************************************
!  SUBROUTINE: UMAT
!****************************************************************************
SUBROUTINE UMAT(STRESS, STATEV, DDSDDE, SSE, SPD, SCD, &
                RPL, DDSDDT, DRPLDE, DRPLDT, STRAN, DSTRAN, &
                TIME, DTIME, TEMP, DTEMP, PREDEF, DPRED, CMNAME, &
                NDI, NSHR, NTENS, NSTATV, PROPS, NPROPS, COORDS, &
                DROT, PNEWDT, CESTRN, DFGRD0, DFGRD1, NOEL, NPT, LAYER, KSPT, KSTEP, KINC)

USE simple_module
USE NetworkManager

INCLUDE 'ABA_PARAM.INC'

CHARACTER*80 CMNAME
INTEGER NDI, NSHR, NTENS, NSTATV, NPROPS, NOEL, NPT, LAYER, KSPT, KSTEP, KINC
DOUBLE PRECISION DTIME, TEMP, DTEMP, PNEWDT, SIG_EQ
DIMENSION STRESS(NTENS), STATEV(NSTATV), DDSDDE(NTENS, NTENS)
DIMENSION STRAN(NTENS), DSTRAN(NTENS), PROPS(NPROPS)
DIMENSION COORDS(3), DROT(3,3), DFGRD0(3,3), DFGRD1(3,3)
DOUBLE PRECISION E, NU, LAMBDA, MU
INTEGER I, J
DOUBLE PRECISION C(6,6)

DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: INPUT_DATA, OUTPUT_DATA, STIFFNESS_OUTPUT
ALLOCATE(INPUT_DATA(NTENS), OUTPUT_DATA(NTENS), STIFFNESS_OUTPUT(21))

CALL InitializeNetworks()

PRINT *, "STEP NO:", KSTEP
PRINT *, "KINC NO:", KINC
PRINT *, "STRAN:"
PRINT '(6F20.6)', (STRAN(I), I = 1, NTENS)
PRINT *, "DSTRAN:"
PRINT '(6F20.6)', (DSTRAN(I), I = 1, NTENS)

! Handle linear elastic behavior for initial step
IF (KSTEP == 1 .AND. KINC == 1) THEN
    PRINT *, "Handling linear elastic behavior."
    E = 210000.0D0  ! Elastic modulus in MPa
    NU = 0.3D0      ! Poisson's ratio
    LAMBDA = (E * NU) / ((1.0D0 + NU) * (1.0D0 - 2.0D0 * NU))
    MU = E / (2.0D0 * (1.0D0 + NU))

    C = 0.0D0
    C(1,1) = LAMBDA + 2.0D0 * MU
    C(2,2) = LAMBDA + 2.0D0 * MU
    C(3,3) = LAMBDA + 2.0D0 * MU
    C(1,2) = LAMBDA
    C(1,3) = LAMBDA
    C(2,3) = LAMBDA
    C(2,1) = LAMBDA
    C(3,1) = LAMBDA
    C(3,2) = LAMBDA
    C(4,4) = MU
    C(5,5) = MU
    C(6,6) = MU

    PRINT *, "Linear Elastic Stiffness Matrix Initialized:"
    DO I = 1, 6
        PRINT '(6F20.4)', (C(I, J), J = 1, 6)
    END DO

    DO I = 1, 6
        DO J = 1, 6
            STRESS(I) = STRESS(I) + C(I, J) * DSTRAN(J)
        END DO
    END DO
        
    PRINT *, "Updated Stress Tensor:"
    PRINT '(6F20.4)', STRESS
ELSE
    PRINT *, "Non Linear step. Proceeding with network-based evaluation."
    DO I = 1, NTENS
        INPUT_DATA(I) = STRAN(I) + DSTRAN(I)
    END DO
    
    PRINT *, "INPUT_DATA:"
    PRINT '(6F20.6)', (INPUT_DATA(I), I = 1, NTENS)

    ! Predict stress using the network
    PRINT *, "Predict Network ID:", NET_ID
    OUTPUT_DATA = EvaluateNetwork(NET_ID, INPUT_DATA)

    DO I = 1, NTENS
        STRESS(I) = OUTPUT_DATA(I)
    END DO

    MAX_STRESS = MAXVAL(STRESS)

    DO I = 1, NTENS
        IF (ABS(STRESS(I)) < MAX_STRESS / 50.0) THEN
            STRESS(I) = 0.0
        END IF
    END DO

    PRINT *, "STRESS:"
    PRINT '(6F20.4)', (STRESS(I), I = 1, NTENS)

    ! Predict stiffness matrix using the network
    PRINT *, "Predict Network ID:", STIFF_ID
    STIFFNESS_OUTPUT = EvaluateNetwork(STIFF_ID, INPUT_DATA)
    C = 0.0D0
    C(1,1) = STIFFNESS_OUTPUT(1)
    C(1,2) = STIFFNESS_OUTPUT(2)
    C(1,3) = STIFFNESS_OUTPUT(3)
    C(1,4) = STIFFNESS_OUTPUT(4)
    C(1,5) = STIFFNESS_OUTPUT(5)
    C(1,6) = STIFFNESS_OUTPUT(6)

    C(2,2) = STIFFNESS_OUTPUT(7)
    C(2,3) = STIFFNESS_OUTPUT(8)
    C(2,4) = STIFFNESS_OUTPUT(9)
    C(2,5) = STIFFNESS_OUTPUT(10)
    C(2,6) = STIFFNESS_OUTPUT(11)

    C(3,3) = STIFFNESS_OUTPUT(12)
    C(3,4) = STIFFNESS_OUTPUT(13)
    C(3,5) = STIFFNESS_OUTPUT(14)
    C(3,6) = STIFFNESS_OUTPUT(15)

    C(4,4) = STIFFNESS_OUTPUT(16)
    C(4,5) = STIFFNESS_OUTPUT(17)
    C(4,6) = STIFFNESS_OUTPUT(18)
    C(5,5) = STIFFNESS_OUTPUT(19)
    C(5,6) = STIFFNESS_OUTPUT(20)
    C(6,6) = STIFFNESS_OUTPUT(21)

    ! Lower Triangle
    C(2,1) = C(1,2)

    C(3,1) = C(1,3)
    C(3,2) = C(2,3)

    C(4,1) = C(1,4)
    C(4,2) = C(2,4)
    C(4,3) = C(3,4)

    C(5,1) = C(1,5)
    C(5,2) = C(2,5)
    C(5,3) = C(3,5)
    C(5,4) = C(4,5)

    C(6,1) = C(1,6)
    C(6,2) = C(2,6)
    C(6,3) = C(3,6)
    C(6,4) = C(4,6)
    C(6,5) = C(5,6)

    !CALL SYMTRI(6, C)

    PRINT *, "Stiffness matrix:"
    DO I = 1, 6
        PRINT '(6F20.4)', (C(I, J), J = 1, 6)
    END DO
END IF

DO I = 1, NTENS
    DO J = 1, NTENS
        DDSDDE(I,J) = C(I,J)
    END DO
END DO

RETURN
END SUBROUTINE UMAT